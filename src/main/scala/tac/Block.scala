package tac

import scalax.collection.edges.DiEdge
import scalax.collection.generic.{AbstractDiEdge, Edge}
import scalax.collection.immutable.Graph
import scalax.collection.io.dot.implicits.*
import scalax.collection.io.dot.*
import scalax.collection.{AnyGraph, GraphLike, immutable as img}
import tac.DominatorCalculationState.{MapType, MapValueType}
import tac.{Label, Tac}

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

class BlockEdge(source: Block, target: Block) extends AbstractDiEdge[Block](source, target) {

}

case class BlockTac(tac: Tac, block: Block) {
  override def toString: String = s"$tac in ${block.label}"
}

//type BlockEdge = DiEdge[Block]

class Block(val label: Label, val tacs: List[Tac]) {

  override def toString = s"Block $label(df=${dominanceFrontier.map(_.label)}, df+=${dominanceFrontierClosure.map(_.label)}, idom=${idom.map(_.label)}, dom=${dominators.map(_.label)})${tacs.mkString("{\n  ", "\n  ", "\n}")}"

  var dominators: Set[Block] = Set.empty

  def strictDominators: Set[Block] = dominators.excl(this)

  var idom: Option[Block] = None

  var dominanceFrontier: Set[Block] = Set.empty

  var dominanceFrontierClosure: Set[Block] = Set.empty

  infix def dom(block: Block): Boolean = block.dominators.contains(this)

  infix def sdom(block: Block): Boolean = (this dom block) && this != block

  def self: Block = this

  //  def fillDefUse(): Unit = {
  //    for tac <- tacs do
  //      tac.sources.foreach(s => s.uses = s.uses.incl(BlockTac(tac, this)))
  //  }
}

extension [N, E <: Edge[N], CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]](graph: GraphLike[N, E, CC]) {
  def traverseDfs(start: graph.NodeT)(f: graph.NodeT => Unit): Unit = {
    val dfsStack = mutable.Stack(graph get start)
    val visited: mutable.Set[graph.NodeT] = mutable.Set.empty
    while dfsStack.nonEmpty do
      val b = dfsStack.pop()
      if visited.add(b) then
        dfsStack.pushAll((graph get b).diSuccessors)
        f(b)
  }
  def traverseBfs(start: graph.NodeT)(f: graph.NodeT => Unit): Unit = {
    val bfsQueue = mutable.Queue(graph get start)
    val visited: mutable.Set[graph.NodeT] = mutable.Set.empty
    while bfsQueue.nonEmpty do
      val b = bfsQueue.dequeue()
      if visited.add(b) then
        bfsQueue.enqueueAll((graph get b).diSuccessors)
        f(b)
  }
  def traverseBfsFold[T](start: graph.NodeT)(initialValue: T)(f: (T, graph.NodeT) => T): T = {
    val bfsQueue = mutable.Queue(graph get start)
    val visited: mutable.Set[graph.NodeT] = mutable.Set.empty
    var value = initialValue
    while bfsQueue.nonEmpty do
      val b = bfsQueue.dequeue()
      if visited.add(b) then
        bfsQueue.enqueueAll((graph get b).diSuccessors)
        value = f(value, b)
    value
  }
}

private val root = DotRootGraph(
  directed = true,
  id = Some("MyDot"),
  attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr("shape", "box")))),
  attrList = List(DotAttr("rank_dir", "TB"))
)


case class DominatorCalculationState[T](map: DominatorCalculationState.MapType[T], fixed: Boolean) {
  @targetName("assign")
  def <<(newMap: MapType[T]): DominatorCalculationState[T] = {
    DominatorCalculationState(newMap, fixed && map == newMap)
  }

  @targetName("assign")
  def <<(newPair: (T, MapValueType[T])): DominatorCalculationState[T] = this << (map + newPair)
}

object DominatorCalculationState {

  type MapValueType[T] = Set[T]
  type MapType[T] = Map[T, MapValueType[T]]

  def empty[T]: DominatorCalculationState[T] = DominatorCalculationState(Map.empty, true)
}

extension [T](value: T) {
  @tailrec
  def iterateTillFixed(f: T => (T, Boolean)): T = {
    val (result, fixed) = f(value)
    if fixed then value else value.iterateTillFixed(f)
  }
}
extension [V](map: Map[V, Set[V]]) {
  def strict: Map[V, Set[V]] = map.map((v, set) => (v, set - v))
}
extension [N, E <: Edge[N], CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]](g: GraphLike[N, E, CC]) {
  private def calculateDominatorsFold(dominatorCalculationState: DominatorCalculationState[g.NodeT], curBlock: g.NodeT): DominatorCalculationState[g.NodeT] = {
    val predDoms = curBlock.diPredecessors.map(dominatorCalculationState.map(_))
    val intersection = if predDoms.isEmpty then Set.empty else predDoms.foldLeft(predDoms.head)(_.intersect(_))
    val newDoms = intersection.incl(curBlock)
    dominatorCalculationState << (curBlock -> newDoms)
  }

  def calculateDominators(startBlock: g.NodeT): DominatorCalculationState.MapType[g.NodeT] = {
    val nodeSet = g.nodes.toSet
    val initialMap = Map.from(g.nodes.map(b => b -> (if b == startBlock then Set(b) else nodeSet)))

    def f(x: DominatorCalculationState[g.NodeT]): (DominatorCalculationState[g.NodeT], Boolean) = {
      val res = g.traverseBfsFold(g get startBlock)(x)(calculateDominatorsFold)
      (res, res.fixed)
    }
    val res: DominatorCalculationState[g.NodeT] = DominatorCalculationState(initialMap, false).iterateTillFixed(f)
    res.map

    //    for b <- g.nodes do
    //      val sdoms = b.strictDominators
    //      b.idom = sdoms.find(d => sdoms.excl(d).forall(!_.strictDominators.contains(d)))
  }
}
extension [CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]](g: GraphLike[Block, BlockEdge, CC]) {
  def makeDomTree(): Graph[Block, BlockEdge] =
    Graph.from(g.nodes.map(_.self), g.nodes.flatMap(b => b.idom.map(BlockEdge(_, b))))


  def calculateDominanceFrontiers(): Unit = {
    for edge <- g.edges do
      var x = edge.source
      while !(x sdom edge.target) do
        x.dominanceFrontier = x.dominanceFrontier.incl(edge.target)
        x = x.idom.get

    calculateDominanceFrontierClosures()
  }

  def calculateDominanceFrontierClosures(): Unit = {
    g.nodes.foreach(b => b.dominanceFrontierClosure = b.dominanceFrontier)
    var fixedPoint = false
    while !fixedPoint do
      fixedPoint = true
      for b <- g.nodes do
        for df <- b.dominanceFrontierClosure do
          val resClosure = b.dominanceFrontierClosure | df.dominanceFrontierClosure
          if resClosure != b.dominanceFrontierClosure then
            fixedPoint = false

          b.dominanceFrontierClosure = resClosure
  }

  private def edgeTransformer(innerEdge: img.Graph[Block, BlockEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
    val edge = innerEdge.outer
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

  def asDot: String = {
    val immutableGraph: img.Graph[Block, BlockEdge] = img.Graph.from(g.nodes.outerIterable, g.edges.outerIterable)
    immutableGraph.toDot(root, edgeTransformer)
  }
}
