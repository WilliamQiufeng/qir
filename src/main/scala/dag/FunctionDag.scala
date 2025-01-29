package dag

import cats.implicits.*
import scalax.collection.immutable.Graph
import semantic.*
import tac.*

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

case class FunctionDag(private val semanticAnalysis: SemanticAnalysisInfo, private val functionDecl: ast.ConcreteFnDecl) {
  private val returnSink: IRSymbol = NormalIRSymbol(Temp(), functionDecl.retTy, "%ret".some)
  private val startBlock: Label = Label()
  private val endBlock: Label = Label()
  val symbolTable: FunctionSymbolTable = {
    val table = functionDecl.args.foldLeft(FunctionSymbolTable())((table, arg) =>
      table.insert(arg.name, NormalIRSymbol(Temp(), arg.ty, arg.name.some)))
    functionDecl.block.declarations.foldLeft(table)((table, declaration) =>
      table.insert(declaration.local.name, NormalIRSymbol(Temp(), declaration.ty, declaration.local.name.some)))
  }
  private val labelSymbolMap: Map[ast.LabelValue, Label] = Map.from(functionDecl.block.labelledBlocks.map(_.name -> Label()))
  private val labelMap: Map[Label, Block] = Map.from(
      functionDecl.block.labelledBlocks.map(labelledBlock =>
        val label = labelSymbolMap(labelledBlock.name)
        label -> makeBlock(label, labelledBlock))
    )
    .updated(startBlock, Block(startBlock, makeUndefinedDeclarationList()))
    .updated(endBlock, Block(endBlock, List(Ret(returnSink))))
  val errors: ArrayBuffer[SemanticError] = ArrayBuffer.empty

  private val edges: Iterable[BlockEdge] = labelMap.values.flatMap { block =>
    block.tacs.lastOption match
      case Some(j: Jump) => j.targets.map(BlockEdge(block, _))
      case _ => Iterable.empty
  }.concat(functionDecl.block.labelledBlocks.headOption map (b => BlockEdge(startBlock, lookupBlock(b.name))))

  // Construct graph
  val graph: Graph[Block, BlockEdge] = Graph.from(Iterable(startBlock, endBlock).map(lookupBlock).concat(labelMap.values), edges)

  private implicit def valueTypeToType(valueType: ast.ValueType): Type =
    semanticAnalysis.lookupValueType(valueType).get

  private implicit def localToTemp(local: ast.Local): IRSymbol = local.name

  private implicit def atomToTemp(atom: ast.Atom): IRSymbol = {
    atom match
      case const: ast.Const => ConstIRSymbol(const, Temp(), semanticAnalysis.lookupConstType(const).get)
      case local: ast.Local => local
  }

  private implicit def stringToTemp(name: String): IRSymbol = {
    symbolTable.lookup(name) match {
      case None => semanticAnalysis.globalSymbolTable.lookup(name) match {
        case None => throw new Exception(s"Local $name not found in $functionDecl")
        case Some(s) => s
      }
      case Some(s) => s
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
    val jumpInstructions: List[Tac] = block.jump match
      case ast.Ret(value) =>
        checkType(returnSink.ty, value.ty)
        List(Move(returnSink, value), Goto(endBlock))
      case ast.Goto(label) => List(Goto(label))
      case ast.Branch(test, trueLabel, falseLabel) => List(Branch(test, trueLabel, falseLabel))

    val tacs = block.stmts.foldRight(jumpInstructions) { (instr, list) =>
      instr match
        case ast.Assign(dst, src) => src match
          case expr: ast.BinaryExpr => expr match
            case ast.AddInt(left, right) =>
              checkType(IntType, dst.ty, left.ty, right.ty)
              BinaryArith(BinaryArithOp.AddI, dst, left, right) :: list
            case ast.SubInt(left, right) =>
              checkType(IntType, dst.ty, left.ty, right.ty)
              BinaryArith(BinaryArithOp.SubI, dst, left, right) :: list
            case ast.MulInt(left, right) =>
              checkType(IntType, dst.ty, left.ty, right.ty)
              BinaryArith(BinaryArithOp.MulI, dst, left, right) :: list
            case ast.DivInt(left, right) =>
              checkType(IntType, dst.ty, left.ty, right.ty)
              BinaryArith(BinaryArithOp.DivI, dst, left, right) :: list
          case value: ast.Local => Move(dst, value) :: list
          case atom: ast.Atom => Move(dst, atom) :: list
          case ast.Call(fn, args) =>
            Call(dst, fn.name, args.map(atomToTemp)) :: list
          case ast.ArrayAccess(offset, from) => ???
        case ast.AssignElement(dst, src) => ???
    }
    Block(label, tacs)
  }

  private def makeUndefinedDeclarationList(): List[Tac] = functionDecl.block.declarations.map(decl =>
    val symbol = symbolTable.lookup(decl.local.name).get
    Move(symbol, ConstIRSymbol(ast.ConstUndefined(decl.ty), Temp(), symbol.ty)))

  def makeInfo: FunctionInfo = FunctionInfo(semanticAnalysis, functionDecl, returnSink, labelMap, labelSymbolMap, graph, startBlock, endBlock, symbolTable)

}
