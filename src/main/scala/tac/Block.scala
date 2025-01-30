package tac

import scalax.collection.edges.DiEdge
import scalax.collection.generic.{AbstractDiEdge, Edge}
import scalax.collection.immutable.Graph
import scalax.collection.io.dot.implicits.*
import scalax.collection.io.dot.*
import scalax.collection.{AnyGraph, GraphLike, immutable as img}
import tac.MapFixedPointState.MapType
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

trait FixedPointState[T] {
  val value: T
  val fixed: Boolean
  @targetName("assign")
  def <<(newValue: T): FixedPointState[T]
}

case class ValueFixedPointState[T](value: T, fixed: Boolean) extends FixedPointState[T] {
  @targetName("assign")
  override def <<(newValue: T): FixedPointState[T] = ValueFixedPointState(newValue, fixed && value == newValue)
}

case class MapFixedPointState[T, V](value: MapType[T, V], fixed: Boolean) extends FixedPointState[MapFixedPointState.MapType[T, V]]{
  @targetName("assign")
  override def <<(newMap: MapType[T ,V]): MapFixedPointState[T ,V] = {
    MapFixedPointState(newMap, fixed && value == newMap)
  }

  @targetName("assign")
  def <<(newPair: (T, V)): MapFixedPointState[T, V] = {
    if value.get(newPair._1).contains(newPair._2) then
      this
    else
      MapFixedPointState(value + newPair, false)
  }
}

object MapFixedPointState {
  type MapType[T, V] = Map[T, V]
  type SetMapType[T] = Map[T, Set[T]]
  type BijectionMapType[T] = Map[T, T]

  def empty[T, V]: MapFixedPointState[T, V] = MapFixedPointState(Map.empty[T, V], true)
}

type SetMapFixedPointState[T] = MapFixedPointState[T, Set[T]]
type BijectionFixedPointState[T] = MapFixedPointState[T, T]

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
  private def calculateDominatorsFold(dominatorCalculationState: SetMapFixedPointState[g.NodeT], curBlock: g.NodeT): SetMapFixedPointState[g.NodeT] = {
    val predDoms = curBlock.diPredecessors.map(dominatorCalculationState.value(_))
    val intersection = if predDoms.isEmpty then Set.empty else predDoms.foldLeft(predDoms.head)(_.intersect(_))
    val newDoms = intersection.incl(curBlock)
    dominatorCalculationState << (curBlock -> newDoms)
  }

  def calculateDominators(startBlock: g.NodeT): MapFixedPointState.SetMapType[g.NodeT] = {
    val nodeSet = g.nodes.toSet
    val initialMap = Map.from(g.nodes.map(b => b -> (if b == startBlock then Set(b) else nodeSet)))

    def f(x: SetMapFixedPointState[g.NodeT]): (SetMapFixedPointState[g.NodeT], Boolean) = {
      val res = g.traverseBfsFold(g get startBlock)(x)(calculateDominatorsFold)
      (res, res.fixed)
    }
    val res: SetMapFixedPointState[g.NodeT] = MapFixedPointState(initialMap, false).iterateTillFixed(f)
    res.value

    //    for b <- g.nodes do
    //      val sdoms = b.strictDominators
    //      b.idom = sdoms.find(d => sdoms.excl(d).forall(!_.strictDominators.contains(d)))
  }
  def calculateIDom(startBlock: g.NodeT): Map[g.NodeT, g.NodeT] =
    calculateIDomFromSdoms(startBlock, g.calculateDominators(startBlock).strict)

  def calculateIDomFromSdoms(startBlock: g.NodeT, sdoms: Map[g.NodeT, Set[g.NodeT]]): Map[g.NodeT, g.NodeT] =
    g.nodes.map(b => {
      val sdom = sdoms(b)
      b -> sdom.find(d => sdom.excl(d).forall(!sdoms(_).contains(d)))
    }).foldLeft(Map.empty[g.NodeT, g.NodeT])((m, p) => m.updatedWith(p._1)(_ => p._2))
    
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
