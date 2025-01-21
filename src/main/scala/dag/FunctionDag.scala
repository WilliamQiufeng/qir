package dag

import ast.{Atom, ConcreteFnDecl, LabelValue, LabelledBlock, Local, ValueType}
import scalax.collection.mutable.Graph
import scalax.collection.immutable as img
import semantic.{ConstIRSymbol, FunctionSymbolTable, GlobalSymbolTable, IRSymbol, IntType, SemanticAnalysis, Temp, Type}
import tac.{BinaryArith, BinaryArithOp, Branch, Call, Goto, Jump, Label, Move, Phi, Ret, Tac}
import scalax.collection.io.dot.*
import implicits.*
import cats.implicits.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class FunctionDag(private val semanticAnalysis: SemanticAnalysis, private val functionDecl: ConcreteFnDecl) {
  private val returnSink: IRSymbol = IRSymbol(Temp(), functionDecl.retTy)
  private val labelMap: mutable.HashMap[Label, Block] = mutable.HashMap.empty
  private val startBlock: Label = addTempBlock(Block(Label(), ArrayBuffer.empty))
  private val endBlock: Label = addTempBlock(Block(Label(), ArrayBuffer(Ret(returnSink))))
  private val labelSymbolMap: mutable.HashMap[LabelValue, Label] = mutable.HashMap.empty
  val symbolTable: FunctionSymbolTable = FunctionSymbolTable()
  val errors: ArrayBuffer[SemanticError] = ArrayBuffer.empty

  // Declare arguments and declaration
  for (arg <- functionDecl.args) yield {
    symbolTable.insert(arg.name, IRSymbol(Temp(), arg.ty))
  }
  for (declaration <- functionDecl.block.declarations) yield {
    symbolTable.insert(declaration.local.name, IRSymbol(Temp(), declaration.ty))
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

  val globals: mutable.Set[IRSymbol] = mutable.Set.empty
  val globalBlocks: mutable.HashMap[IRSymbol, mutable.Set[Block]] = mutable.HashMap.empty
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
    for symbol <- symbolTable.values do
      var f: Set[Block] = Set.empty
      var w: Set[Block] = Set.from(symbol.defs.map(_.block))
      while w.nonEmpty do
        val x = w.head
        w = w.excl(x)
        for y <- x.dominanceFrontier do
          if !f.contains(y) then
            val sources = Array.tabulate((graph get y).diPredecessors.size)(_ => symbol)
            y.tacs.insert(0, Phi(symbol, sources))
            f = f.incl(y)
            if !symbol.defs.exists(_.block == y) then
              w = w.incl(y)
  }

  private def rename(b: Block): Unit = {
    for i <- b.tacs do
      i match
        case phi: Phi => i.definition = newName(i.definition.get).some
        case _ =>

    for i <- b.tacs.filterNot(_.isInstanceOf[Phi]) do
      i.sources.mapInPlace { src =>
        if globals.contains(src) && src.stack.nonEmpty then
          src.stack.top
        else
          src
      }
      if i.definition.isDefined && globals.contains(i.definition.get) then
        i.definition = newName(i.definition.get).some

    for s <- (graph get b).diSuccessors do
      for i <- s.tacs do
        i match
          case phi: Phi =>
            var changed = false
            for srcIdx <- phi.sources.indices do
              val src = phi.sources(srcIdx)
              if !changed && globals.contains(src) && src.stack.nonEmpty then
                phi.sources.update(srcIdx, src.stack.top)
                changed = true
          case _ =>

    for s <- (dominatorTree get b).diSuccessors do
      rename(s)

    for i <- b.tacs do
      i.definition.foreach(d => if d.stack.nonEmpty then d.stack.pop())
  }

  private def newName(symbol: IRSymbol): IRSymbol = {
    val newSym = IRSymbol(Temp(), symbol.ty)
    symbol.stack.push(newSym)
    newSym
  }

  //  private def rename(): Unit = {
  //    symbolTable.values.foreach(_.reachingDef = None)
  //
  //    dominatorTree.traverseDfs(dominatorTree get startBlock, b =>
  //      for i <- b.tacs do
  //        if !i.isInstanceOf[Phi] then
  //          i.sources.mapInPlace(v =>
  //            updateReachingDef(v, b)
  //            v.reachingDef.get
  //          )
  //
  //        for v <- i.definition do
  //          updateReachingDef(v, b)
  //          val newV = IRSymbol(Temp(), v.ty)
  //          i.definition = newV.some
  //          newV.reachingDef = v.reachingDef
  //          v.reachingDef = newV.some
  //
  //      for phi <- b.diSuccessors.flatMap(_.tacs.filter(_.isInstanceOf[Phi])) do
  //        phi.sources.mapInPlace(v =>
  //          updateReachingDef(v, b)
  //          v.reachingDef.get
  //        )
  //    )
  //  }
  //
  //  private def updateReachingDef(v: IRSymbol, ib: Block): Unit = {
  //    var r = v.reachingDef
  //    while !(r.isEmpty || r.get.defs.exists(d => d.block dom ib)) do
  //      r = r.flatMap(_.reachingDef)
  //    v.reachingDef = r
  //  }
}
