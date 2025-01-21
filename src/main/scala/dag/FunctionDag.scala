package dag

import ast.{Atom, ConcreteFnDecl, LabelValue, LabelledBlock, Local, ValueType}
import scalax.collection.mutable.Graph
import scalax.collection.immutable as img
import semantic.{FunctionSymbolTable, GlobalSymbolTable, IRSymbol, SemanticAnalysis, Temp, Type}
import tac.{BinaryArith, BinaryArithOp, Branch, Call, Goto, Jump, Label, Move, Ret, Tac}
import scalax.collection.io.dot.*

import implicits._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class FunctionDag(private val semanticAnalysis: SemanticAnalysis, private val functionDecl: ConcreteFnDecl) {
  private val returnSink: IRSymbol = IRSymbol(Temp(), functionDecl.retTy)
  private val labelMap: mutable.HashMap[Label, Block] = mutable.HashMap.empty
  private val startBlock: Label = addTempBlock(Block(ArrayBuffer.empty))
  private val endBlock: Label = addTempBlock(Block(ArrayBuffer(Ret(returnSink))))
  private val labelSymbolMap: mutable.HashMap[LabelValue, Label] = mutable.HashMap.empty
  private val symbolTable: FunctionSymbolTable = FunctionSymbolTable()

  for (declaration <- functionDecl.block.declarations) yield {
    symbolTable.insert(declaration.local.name, IRSymbol(Temp(), declaration.ty))
  }

  for (labelledBlock <- functionDecl.block.labelledBlocks) yield {
    labelSymbolMap.addOne(labelledBlock.name, Label())
  }

  for (labelledBlock <- functionDecl.block.labelledBlocks) yield {
    val label = labelSymbolMap(labelledBlock.name)
    labelMap.addOne(label, makeBlock(labelledBlock))
  }

  private val graph: Graph[Block, BlockEdge] = Graph.from(Iterable(startBlock, endBlock).map(lookupBlock).concat(labelMap.values), Iterable.empty)

  for (block <- labelMap.values) yield {
    block.tacs.lastOption match
      case Some(j: Jump) => j.targets.foreach(t => graph.addOne(BlockEdge(block, t)))
      case _ =>
  }

  functionDecl.block.labelledBlocks.headOption match {
    case Some(b: LabelledBlock) => graph.addOne(BlockEdge(startBlock, lookupBlock(b.name)))
    case _ =>
  }

  private val root = DotRootGraph(
    directed = true,
    id = Some("MyDot"),
  )

  private def edgeTransformer(innerEdge: img.Graph[Block, BlockEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
    val edge = innerEdge.outer
    //    val label = edge.label
    val label = ""
    Some(
      root,
      DotEdgeStmt(
        NodeId(edge.source.toString),
        NodeId(edge.target.toString),
        if (label.nonEmpty) List(DotAttr(Id("label"), Id(label)))
        else Nil
      )
    )
  }

  def toDot: String = {
    val g: img.Graph[Block, BlockEdge] = img.Graph.from(graph.nodes.outerIterable, graph.edges.outerIterable)
    g.toDot(root, edgeTransformer)
  }

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

  private def makeBlock(block: ast.LabelledBlock): Block = {
    val arr: ArrayBuffer[Tac] = ArrayBuffer.empty
    for (instr <- block.stmts) yield {
      instr match
        case ast.Assign(dst, src) => src match
          case expr: ast.BinaryExpr => expr match
            case ast.AddInt(left, right) => arr.addOne(BinaryArith(BinaryArithOp.AddI, dst, left, right))
            case ast.SubInt(left, right) => arr.addOne(BinaryArith(BinaryArithOp.SubI, dst, left, right))
            case ast.MulInt(left, right) => arr.addOne(BinaryArith(BinaryArithOp.MulI, dst, left, right))
            case ast.DivInt(left, right) => arr.addOne(BinaryArith(BinaryArithOp.DivI, dst, left, right))
          case value: Local => arr.addOne(Move(dst, value))
          case atom: ast.Atom => arr.addOne(Move(dst, atom))
          case ast.Call(fn, args) => arr.addOne(Call(dst, fn.name, args.map(atomToTemp)))
          case ast.ArrayAccess(offset, from) => ???
        case ast.AssignElement(dst, src) => ???
    }

    val jump = block.jump
    jump match
      case ast.Ret(value) =>
        arr.addOne(Move(returnSink, value))
        arr.addOne(Goto(endBlock))
      case ast.Goto(label) => arr.addOne(Goto(label))
      case ast.Branch(test, trueLabel, falseLabel) => arr.addOne(Branch(test, trueLabel, falseLabel))
    Block(arr)
  }

  private def addTempBlock(block: Block): Label = {
    val label = Label()
    labelMap.addOne(label, block)
    label
  }
}
