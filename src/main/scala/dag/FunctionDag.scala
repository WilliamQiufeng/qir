package dag

import ast.{Atom, ConcreteFnDecl, LabelValue, LabelledBlock, Local, ValueType}
import scalax.collection.mutable.Graph
import scalax.collection.immutable.Graph as ImmutableGraph
import semantic.{ConstIRSymbol, FunctionSymbolTable, GlobalSymbolTable, IRSymbol, IntType, SSASymbol, SemanticAnalysis, Temp, Type}
import tac.{BinaryArith, BinaryArithOp, Block, BlockEdge, Branch, Call, Goto, Jump, Label, Move, Phi, Ret, Tac}
import scalax.collection.io.dot.*
import implicits.*
import cats.implicits.*
import ssa.FunctionSsaPass

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class FunctionDag(private val semanticAnalysis: SemanticAnalysis, private val functionDecl: ConcreteFnDecl) {
  private val returnSink: IRSymbol = IRSymbol(Temp(), functionDecl.retTy, "%ret".some, true)
  private val labelMap: mutable.HashMap[Label, Block] = mutable.HashMap.empty
  private val startBlock: Label = addTempBlock(Block(Label(), ArrayBuffer.empty))
  private val endBlock: Label = addTempBlock(Block(Label(), ArrayBuffer(Ret(returnSink))))
  private val labelSymbolMap: mutable.HashMap[LabelValue, Label] = mutable.HashMap.empty
  val symbolTable: FunctionSymbolTable = FunctionSymbolTable()
  val errors: ArrayBuffer[SemanticError] = ArrayBuffer.empty

  // Declare arguments and declaration
  for (arg <- functionDecl.args) yield {
    symbolTable.insert(arg.name, IRSymbol(Temp(), arg.ty, arg.name.some))
  }
  for (declaration <- functionDecl.block.declarations) yield {
    symbolTable.insert(declaration.local.name, IRSymbol(Temp(), declaration.ty, declaration.local.name.some, true))
  }

  // Form labels and blocks
  for (labelledBlock <- functionDecl.block.labelledBlocks) yield {
    labelSymbolMap.addOne(labelledBlock.name, Label())
  }

  for (labelledBlock <- functionDecl.block.labelledBlocks) yield {
    val label = labelSymbolMap(labelledBlock.name)
    labelMap.addOne(label, makeBlock(label, labelledBlock))
  }

  private val edges: Iterable[BlockEdge] = labelMap.values.flatMap { block =>
    block.tacs.lastOption match
      case Some(j: Jump) => j.targets.map(BlockEdge(block, _))
      case _ => Iterable.empty
  }.concat(functionDecl.block.labelledBlocks.headOption map (b => BlockEdge(startBlock, lookupBlock(b.name))))

  // Construct graph
  val graph: Graph[Block, BlockEdge] = Graph.from(Iterable(startBlock, endBlock).map(lookupBlock).concat(labelMap.values), edges)

  private implicit def valueTypeToType(valueType: ValueType): Type =
    semanticAnalysis.translateValueType(valueType).toOption.get

  private implicit def localToTemp(local: Local): IRSymbol = local.name

  private implicit def atomToTemp(atom: Atom): IRSymbol = {
    atom match
      case const: ast.Const => semanticAnalysis.getOrAddConst(const)
      case local: Local => local
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

  private implicit def lookupLabelValue(labelValue: LabelValue): Label = labelSymbolMap(labelValue)

  private def checkType(desired: Type, checkTypes: Type*) = {
    checkTypes.find(_ != desired) match {
      case Some(mismatchedType) => errors.addOne(TypeMismatch(desired, mismatchedType))
      case _ =>
    }
  }

  private def makeBlock(label: Label, block: ast.LabelledBlock): Block = {
    val arr: ArrayBuffer[Tac] = ArrayBuffer.empty
    for (instr <- block.stmts) yield {
      instr match
        case ast.Assign(dst, src) => src match
          case expr: ast.BinaryExpr => expr match
            case ast.AddInt(left, right) =>
              checkType(IntType, dst.ty, left.ty, right.ty)
              arr.addOne(BinaryArith(BinaryArithOp.AddI, dst, left, right))
            case ast.SubInt(left, right) =>
              checkType(IntType, dst.ty, left.ty, right.ty)
              arr.addOne(BinaryArith(BinaryArithOp.SubI, dst, left, right))
            case ast.MulInt(left, right) =>
              checkType(IntType, dst.ty, left.ty, right.ty)
              arr.addOne(BinaryArith(BinaryArithOp.MulI, dst, left, right))
            case ast.DivInt(left, right) =>
              checkType(IntType, dst.ty, left.ty, right.ty)
              arr.addOne(BinaryArith(BinaryArithOp.DivI, dst, left, right))
          case value: Local => arr.addOne(Move(dst, value))
          case atom: ast.Atom => arr.addOne(Move(dst, atom))
          case ast.Call(fn, args) =>
            arr.addOne(Call(dst, fn.name, args.map(atomToTemp)))
          case ast.ArrayAccess(offset, from) => ???
        case ast.AssignElement(dst, src) => ???
    }

    val jump = block.jump
    jump match
      case ast.Ret(value) =>
        checkType(returnSink.ty, value.ty)
        arr.addOne(Move(returnSink, value))
        arr.addOne(Goto(endBlock))
      case ast.Goto(label) => arr.addOne(Goto(label))
      case ast.Branch(test, trueLabel, falseLabel) => arr.addOne(Branch(test, trueLabel, falseLabel))
    Block(label, arr)
  }

  private def addTempBlock(block: Block): Label = {
    labelMap.addOne(block.label, block)
    block.label
  }

  def makeInfo: FunctionInfo = FunctionInfo(semanticAnalysis, functionDecl, returnSink, labelMap, labelSymbolMap, graph, startBlock, endBlock, symbolTable)

}
