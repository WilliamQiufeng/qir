package dag

import ast.{Atom, ConcreteFnDecl, LabelValue, LabelledBlock, Local, ValueType}
import scalax.collection.mutable.Graph
import scalax.collection.immutable as img
import semantic.{ConstIRSymbol, FunctionSymbolTable, GlobalSymbolTable, IRSymbol, IntType, SSASymbol, SemanticAnalysis, Temp, Type}
import tac.{BinaryArith, BinaryArithOp, Branch, Call, Goto, Jump, Label, Move, Phi, Ret, Tac}
import scalax.collection.io.dot.*
import implicits.*
import cats.implicits.*

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

  // Construct graph
  val graph: Graph[Block, BlockEdge] = Graph.from(Iterable(startBlock, endBlock).map(lookupBlock).concat(labelMap.values), Iterable.empty)

  // Add edges
  for (block <- labelMap.values) yield {
    block.tacs.lastOption match
      case Some(j: Jump) => j.targets.foreach(t => graph.addOne(BlockEdge(block, t)))
      case _ =>
  }

  // Add edge start block -> first block
  functionDecl.block.labelledBlocks.headOption match {
    case Some(b: LabelledBlock) => graph.addOne(BlockEdge(startBlock, lookupBlock(b.name)))
    case _ =>
  }

  graph.calculateDominators(startBlock)

  graph.calculateDominanceFrontiers()

  val dominatorTree: Graph[Block, BlockEdge] = graph.makeDomTree()

  private val globals: mutable.Set[IRSymbol] = mutable.Set.empty
  private val globalBlocks: mutable.HashMap[IRSymbol, mutable.Set[Block]] = mutable.HashMap.empty
  for b <- graph.nodes do
    val varKill: mutable.Set[IRSymbol] = mutable.Set.empty
    for i <- b.tacs do
      globals.addAll(i.sources.filterNot(varKill.contains))
      i.definition.foreach(dst =>
        varKill.addOne(dst)
        globalBlocks.getOrElseUpdate(dst, mutable.Set.empty).addOne(b)
      )

  insertPhi()

  rename(startBlock)

  graph.nodes.foreach(_.fillDefUse())


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

  private def insertPhi(): Unit = {
    for x <- globals do
      val insertedBlocks: mutable.Set[Block] = mutable.Set.empty
      val workList: mutable.Set[Block] = globalBlocks.getOrElse(x, mutable.Set.empty)
      while workList.nonEmpty do
        val b = workList.head
        workList -= b
        for d <- b.dominanceFrontier do
          if insertedBlocks.add(d) then
            val preds = (graph get d).diPredecessors.map(_.self).toArray
            val sources = preds.map(_ => x)
            d.tacs.insert(0, Phi(x, sources, preds))
            workList += d
  }

  private def rename(b: Block): Unit = {
    val symbolsOverwritten: mutable.Set[IRSymbol] = mutable.Set.empty

    // We don't need to keep track of more than one rewritten variables
    def newName(symbol: IRSymbol): IRSymbol = {
      val newSym = SSASymbol(symbol)
      if !symbolsOverwritten.add(symbol) then
        symbol.stack.pop()
      symbol.stack.push(newSym)
      newSym
    }

    // Give phi targets a new name
    for i <- b.tacs do
      i match
        case _: Phi => i.definition = newName(i.definition.get).some
        case _ =>

    // Rewrite sources and make new name for targets
    b.tacs.foreach {
      case _: Phi =>
      case i =>
        i.sources.mapInPlace { src =>
          if globals.contains(src) && src.stack.nonEmpty then
            src.stack.top
          else
            src
        }
        if i.definition.isDefined && globals.contains(i.definition.get) then
          i.definition = newName(i.definition.get).some
    }

    (graph get b).diSuccessors.flatMap(_.tacs).foreach {
      case phi: Phi => phi.replace(b, src => if src.stack.nonEmpty then src.stack.top else src)
      case _ =>
    }

    for s <- (dominatorTree get b).diSuccessors do
      rename(s)

    symbolsOverwritten.foreach(_.stack.pop())
  }
}
