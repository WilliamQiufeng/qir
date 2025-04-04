package dag

import cats.syntax.all.{catsSyntaxApplicativeId, catsSyntaxOptionId}
import common.{CompilerContext, FunctionPass, FunctionPassResult}
import dag.FunctionDag.makeFlowGraph
import mem.CDecl
import scalax.collection.immutable.Graph
import semantic.*
import tac.*

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

case object FunctionDagGenerationPass extends FunctionPass[ast.ConcreteFnDecl, FunctionInfo] {
  override def apply(in: ast.ConcreteFnDecl)(implicit ctx: CompilerContext): FunctionPassResult[FunctionInfo] =
    FunctionDag(ctx.semanticAnalysisInfo, in).makeInfo.pure
}

case class FunctionDag(private val semanticAnalysis: SemanticAnalysisInfo, private val functionDecl: ast.ConcreteFnDecl) {
  private val returnSink: Temp = Temp.make
  private val startBlock: Label = Label()
  private val endBlock: Label = Label()
  private val arguments: List[Temp] = functionDecl.args.map(_ => Temp.make)
  val symbolTable: FunctionSymbolTable = {
    val table = functionDecl.args.zip(arguments).foldLeft(FunctionSymbolTable())((table, arg) =>
      table.insert(arg._1.name, NormalIRSymbol(arg._2, arg._1.ty, arg._1.name.some)))
    functionDecl.block.declarations.foldLeft(table)((table, declaration) => declaration match
      case ast.Declaration(local, ty) => table.insert(
        local.name,
        NormalIRSymbol(Temp.make, ty, local.name.some, true)
      )

      case ast.FunctionConstDecl(local, const) => table.insert(
        local.name,
        ConstIRSymbol(const, Temp.make, semantic.lookupConstType[Option](const).get)
      )
    )
  }
  private val tempMap: Map[Temp, IRSymbol] = symbolTable.map.map(p => p._2.temp -> p._2) ++ semanticAnalysis.constTempMap +
    (returnSink -> NormalIRSymbol(returnSink, functionDecl.retTy, "%ret".some))
  private val labelSymbolMap: Map[ast.LabelValue, Label] = Map.from(functionDecl.block.labelledBlocks.map(_.name -> Label()))
  private val labelMap: Map[Label, NormalBlock] = {
    val userDefinedLabels = Map.from(
      functionDecl.block.labelledBlocks.map(labelledBlock =>
        val label = labelSymbolMap(labelledBlock.name)
        label -> makeBlock(label, labelledBlock))
    )
    userDefinedLabels
      .updated(startBlock, NormalBlock(startBlock, List(), Goto(labelSymbolMap(functionDecl.block.labelledBlocks.head.name))))
      .updated(endBlock, NormalBlock(endBlock, List(), Ret(returnSink)))
  }
  val errors: ArrayBuffer[SemanticError] = ArrayBuffer.empty


  // Construct graph
  val graph: Graph[Label, LabelEdge] = makeFlowGraph(labelMap, startBlock)

  private implicit def valueTypeToType(valueType: ast.ValueType): Type =
    semanticAnalysis.lookupValueType(valueType).get

  private implicit def localToTemp(local: ast.Local): Temp = local.name

  private implicit def stringToTemp(name: String): Temp = {
    symbolTable.lookup(name) match {
      case None => semanticAnalysis.globalSymbolTable.lookup(name) match {
        case None => throw new Exception(s"Local $name not found in $functionDecl")
        case Some(s) => s.temp
      }
      case Some(s) => s.temp
    }
  }

  private implicit def lookupBlock(label: Label): NormalBlock = labelMap(label)

  private implicit def lookupLabelValue(labelValue: ast.LabelValue): Label = labelSymbolMap(labelValue)

  private def checkType(desired: Type, checkTypes: Type*) = {
    checkTypes.find(_ != desired) match {
      case Some(mismatchedType) => errors.addOne(TypeMismatch(desired, mismatchedType))
      case _ =>
    }
  }

  private def makeBlock(label: Label, block: ast.LabelledBlock): NormalBlock = {
    val (initialTacList, terminator) = {
      block.jump match
        case ast.Ret(value) => (List[NormalTac](Move(returnSink, value)), Goto(endBlock))
        case ast.Goto(label) => (List.empty, Goto(label))
        case ast.Branch(test, trueLabel, falseLabel) => (List.empty, Branch(test, trueLabel, falseLabel))
    }

    val tacs = block.stmts.foldRight(initialTacList) { (instr, list) =>
      instr match
        case ast.Assign(dst, src) => src match
          case expr: ast.BinaryExpr => expr match
            case ast.AddInt(left, right) =>
              BinaryArith(dst, left, right, BinaryArithOp.AddI) :: list
            case ast.SubInt(left, right) =>
              BinaryArith(dst, left, right, BinaryArithOp.SubI) :: list
            case ast.MulInt(left, right) =>
              BinaryArith(dst, left, right, BinaryArithOp.MulI) :: list
            case ast.DivInt(left, right) =>
              BinaryArith(dst, left, right, BinaryArithOp.DivI) :: list
          case value: ast.Local => Move(dst, value) :: list
          case ast.Call(fn, args) =>
            Call(dst, IndexedSeq.from(args.map(localToTemp)), fn.name) :: list
          case ast.ArrayAccess(offset, from) => ???
        case ast.AssignElement(dst, src) => ???
    }
    NormalBlock(label, tacs, terminator)
  }

  def makeInfo: FunctionInfo = FunctionInfo(functionDecl, returnSink, labelMap, labelSymbolMap, startBlock, endBlock, symbolTable, graph, tempMap,
    FunctionHeader(CDecl, arguments))
}

object FunctionDag {
  def makeFlowGraph(labelMap: Map[Label, Block], startBlock: Label): Graph[Label, LabelEdge] = {
    val edges = labelMap.values.flatMap { block =>
      block.terminator.targets.map(LabelEdge(block.label, _))
    }
    val entryBlock = labelMap(startBlock).terminator.targets.headOption
    // Construct graph
    Graph.from(labelMap.keys, entryBlock match {
      case None => edges
      case Some(entry) => edges.toIndexedSeq :+ LabelEdge(startBlock, entry)
    })
  }
}
