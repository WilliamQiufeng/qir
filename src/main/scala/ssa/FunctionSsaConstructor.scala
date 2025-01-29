package ssa

import dag.*
import scalax.collection.immutable.Graph as ImmutableGraph
import semantic.{IRSymbol, SSASymbol}
import tac.Phi
import cats.implicits.*
import scala.collection.mutable

class FunctionSsaConstructor(functionInfo: FunctionInfo) extends FunctionPass(functionInfo) {

  private val globals: Set[IRSymbol] = {
    var res: Set[IRSymbol] = Set.empty
    for b <- functionInfo.flowGraph.nodes do
      val varKill: mutable.Set[IRSymbol] = mutable.Set.empty
      for i <- b.tacs do
        res = res | i.sources.filterNot(varKill.contains).toSet
    res
  }
  private val globalBlocks: Map[IRSymbol, Set[Block]] = {
    val res = mutable.HashMap.empty[IRSymbol, Set[Block]]
    for b <- functionInfo.flowGraph.nodes do
      val varKill: mutable.Set[IRSymbol] = mutable.Set.empty
      for i <- b.tacs do
        i.definition.foreach(dst =>
          varKill.addOne(dst)
          res.updateWith(dst) {
            case None => Some(Set.empty)
            case Some(s) => Some(s + b)
          }
        )
    res.toMap
  }

  private def insertPhi(): Unit = {
    for x <- globals do
      val insertedBlocks: mutable.Set[Block] = mutable.Set.empty
      val workList: mutable.Set[Block] = mutable.Set.from(globalBlocks.getOrElse(x, Set.empty))
      while workList.nonEmpty do
        val b = workList.head
        workList -= b
        for d <- b.dominanceFrontier do
          if insertedBlocks.add(d) then
            val preds = (functionInfo.flowGraph get d).diPredecessors.map(_.self).toArray
            val sources = preds.map(_ => x)
            d.tacs.insert(0, Phi(x, sources, preds))
            workList += d
  }

  private def rename(dominatorTree: ImmutableGraph[Block, BlockEdge], b: Block): Unit = {
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

    (functionInfo.flowGraph get b).diSuccessors.flatMap(_.tacs).foreach {
      case phi: Phi => phi.replace(b, src => if src.stack.nonEmpty then src.stack.top else src)
      case _ =>
    }

    for s <- (dominatorTree get b).diSuccessors do
      rename(dominatorTree, s)

    symbolsOverwritten.foreach(_.stack.pop())
  }

  override def perform: FunctionInfo = {
    functionInfo.flowGraph.calculateDominators(functionInfo.startBlock)

    functionInfo.flowGraph.calculateDominanceFrontiers()
    val dominatorTree = functionInfo.flowGraph.makeDomTree()

    insertPhi()

    rename(dominatorTree, functionInfo.startBlock)

    functionInfo.flowGraph.nodes.foreach(_.fillDefUse())
    functionInfo
  }

}
