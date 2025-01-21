package dag

import scalax.collection.edges.DiEdge
import scalax.collection.generic.{AbstractDiEdge, Edge}
import scalax.collection.mutable.Graph
import tac.{Label, Tac}

import scala.collection.mutable

class BlockEdge(source: Block, target: Block) extends AbstractDiEdge[Block](source, target) {

}

//type BlockEdge = DiEdge[Block]

class Block(val label: Label, val tacs: mutable.ArrayBuffer[Tac]) {
  override def toString = s"Block $label(df=${dominanceFrontier.map(_.label)}, idom=${idom.map(_.label)}, dom=${dominators.map(_.label)})${tacs.mkString("{\n  ", "\n  ", "\n}")}"

  var dominators: Set[Block] = Set.empty

  def strictDominators: Set[Block] = dominators.excl(this)

  var idom: Option[Block] = None

  var dominanceFrontier: Set[Block] = Set.empty

  infix def dom(block: Block): Boolean = block.dominators.contains(this)

  infix def sdom(block: Block): Boolean = (this dom block) && this != block

  def self: Block = this
}

extension (g: Graph[Block, BlockEdge]) {
  def calculateDominators(startBlock: Block): Unit = {
    val nodeSet = g.nodes.map(_.self).toSet
    g.nodes.foreach(b => b.dominators = if b == startBlock then Set(b) else nodeSet)

    var fixedPoint = false
    while !fixedPoint do
      fixedPoint = true
      val queue = mutable.Queue(g get startBlock)
      val visited: mutable.Set[Block] = mutable.Set.empty
      while queue.nonEmpty do
        val curBlock = queue.dequeue()
        if visited.add(curBlock) then
          queue.addAll(curBlock.diSuccessors)

          val predDoms = curBlock.diPredecessors.map(_.dominators)
          val intersection = if predDoms.isEmpty then Set.empty else predDoms.foldLeft(predDoms.head)(_.intersect(_))
          val newDoms = intersection.incl(curBlock)
          if !(curBlock.dominators equals newDoms) then
            fixedPoint = false

          curBlock.dominators = newDoms

    for b <- g.nodes do
      val sdoms = b.strictDominators
      b.idom = sdoms.find(d => sdoms.excl(d).forall(!_.strictDominators.contains(d)))
  }

  def calculateDominanceFrontiers(): Unit = {
    for edge <- g.edges do
      var x = edge.source
      while !(x sdom edge.target) do
        x.dominanceFrontier = x.dominanceFrontier.incl(edge.target)
        x = x.idom.get
  }
}
