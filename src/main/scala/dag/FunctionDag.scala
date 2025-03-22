package dag

import common.{CompilerContext, FunctionPass, FunctionPassResult}
import scalax.collection.immutable.Graph
import semantic.*
import tac.*
import cats.syntax.all.catsSyntaxOptionId
import cats.syntax.all.catsSyntaxApplicativeId
import mem.CDecl

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

case object FunctionDagGenerationPass extends FunctionPass[ast.ConcreteFnDecl, FunctionInfo] {
  override def apply(in: ast.ConcreteFnDecl)(implicit ctx: CompilerContext): FunctionPassResult[FunctionInfo] =
    FunctionDag(ctx.semanticAnalysisInfo, in).makeInfo.pure
}

case class FunctionDag(private val semanticAnalysis: SemanticAnalysisInfo, private val functionDecl: ast.ConcreteFnDecl) {
  private val returnSink: Temp = Temp()
  private val startBlock: Label = Label()
  private val endBlock: Label = Label()
  private val arguments: List[Temp] = functionDecl.args.map(_ => Temp())
  val symbolTable: FunctionSymbolTable = {
    val table = functionDecl.args.zip(arguments).foldLeft(FunctionSymbolTable())((table, arg) =>
      table.insert(arg._1.name, NormalIRSymbol(arg._2, arg._1.ty, arg._1.name.some)))
    functionDecl.block.declarations.foldLeft(table)((table, declaration) => declaration match
      case ast.Declaration(local, ty) => table.insert(
        local.name,
        NormalIRSymbol(Temp(), ty, local.name.some, true)
      )

      case ast.FunctionConstDecl(local, const) => table.insert(
        local.name,
        ConstIRSymbol(const, Temp(), semantic.lookupConstType[Option](const).get)
      )
    )
  }
  private val tempMap: Map[Temp, IRSymbol] = symbolTable.map.map(p => p._2.temp -> p._2) ++ semanticAnalysis.constTempMap +
    (returnSink -> NormalIRSymbol(returnSink, functionDecl.retTy, "%ret".some))
  private val labelSymbolMap: Map[ast.LabelValue, Label] = Map.from(functionDecl.block.labelledBlocks.map(_.name -> Label()))
  private val labelMap: Map[Label, Block] = Map.from(
      functionDecl.block.labelledBlocks.map(labelledBlock =>
        val label = labelSymbolMap(labelledBlock.name)
        label -> makeBlock(label, labelledBlock))
    )
    .updated(startBlock, Block(startBlock, List()))
    .updated(endBlock, Block(endBlock, List(Tac(IndexedSeq(returnSink), None, Ret))))
  val errors: ArrayBuffer[SemanticError] = ArrayBuffer.empty

  private val edges: Iterable[LabelEdge] = labelMap.values.flatMap { block =>
    block.tacs.lastOption match
      case Some(t) => t.impl match
        case j: Jump => j.targets.map(LabelEdge(block.label, _))
        case _ => Iterable.empty
      case _ => Iterable.empty
  }.concat(functionDecl.block.labelledBlocks.headOption map (b => LabelEdge(startBlock, b.name)))

  // Construct graph
  val graph: Graph[Label, LabelEdge] = Graph.from(labelMap.keys, edges)

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

  private implicit def lookupBlock(label: Label): Block = labelMap(label)

  private implicit def lookupLabelValue(labelValue: ast.LabelValue): Label = labelSymbolMap(labelValue)

  private def checkType(desired: Type, checkTypes: Type*) = {
    checkTypes.find(_ != desired) match {
      case Some(mismatchedType) => errors.addOne(TypeMismatch(desired, mismatchedType))
      case _ =>
    }
  }

  private def makeBlock(label: Label, block: ast.LabelledBlock): Block = {
    val jumpInstructions: List[Tac[TacImpl]] = block.jump match
      case ast.Ret(value) => List(Tac(IndexedSeq(value), returnSink.some, Move), Tac(IndexedSeq.empty, None, Goto(endBlock)))
      case ast.Goto(label) => List(Tac(IndexedSeq.empty, None, Goto(label)))
      case ast.Branch(test, trueLabel, falseLabel) => List(Tac(IndexedSeq(test), None, Branch(trueLabel, falseLabel)))

    val tacs = block.stmts.foldRight(jumpInstructions) { (instr, list) =>
      instr match
        case ast.Assign(dst, src) => src match
          case expr: ast.BinaryExpr => expr match
            case ast.AddInt(left, right) =>
              Tac(IndexedSeq(left, right), Some(dst), BinaryArith(BinaryArithOp.AddI)) :: list
            case ast.SubInt(left, right) =>
              Tac(IndexedSeq(left, right), Some(dst), BinaryArith(BinaryArithOp.SubI)) :: list
            case ast.MulInt(left, right) =>
              Tac(IndexedSeq(left, right), Some(dst), BinaryArith(BinaryArithOp.MulI)) :: list
            case ast.DivInt(left, right) =>
              Tac(IndexedSeq(left, right), Some(dst), BinaryArith(BinaryArithOp.DivI)) :: list
          case value: ast.Local => Tac(IndexedSeq(value), Some(dst), Move) :: list
          case ast.Call(fn, args) =>
            Tac(IndexedSeq.from(args.map(localToTemp)), Some(dst), Call(fn.name)) :: list
          case ast.ArrayAccess(offset, from) => ???
        case ast.AssignElement(dst, src) => ???
    }
    Block(label, tacs)
  }

  def makeInfo: FunctionInfo = FunctionInfo(functionDecl, returnSink, labelMap, labelSymbolMap, startBlock, endBlock, symbolTable, graph, tempMap,
    FunctionHeader(CDecl, arguments))
}
