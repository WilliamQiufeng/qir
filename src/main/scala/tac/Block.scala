package tac

import scalax.collection.generic.Edge
import scalax.collection.immutable.Graph
import scalax.collection.io.dot.*
import scalax.collection.io.dot.implicits.*
import scalax.collection.{AnyGraph, GraphLike, immutable as img}

import scala.collection.View


case class BlockTac(tac: Tac, block: NormalBlock) {
  override def toString: String = s"$tac in ${block.label}"
}

//type BlockEdge = DiEdge[Block]
trait Block {
  def label: Label
  def tacs: View[Tac]
  def terminator: Terminator
}
case class NormalBlock(label: Label, normalTacs: List[NormalTac], terminator: Terminator) extends Block {
  override def tacs: View[Tac] = normalTacs.view.appended(terminator)
  override def toString: String = s"Block $label${tacs.mkString("{\n  ", "\n  ", "\n}")}"

  def self: NormalBlock = this

  //  def fillDefUse(): Unit = {
  //    for tac <- tacs do
  //      tac.sources.foreach(s => s.uses = s.uses.incl(BlockTac(tac, this)))
  //  }
}


private val root = DotRootGraph(
  directed = true,
  id = Some("MyDot"),
  attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr("shape", "box")))),
  attrList = List(DotAttr("rank_dir", "TB"))
)
extension [N, E <: Edge[N], CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]](g: GraphLike[N, E, CC]) {

  private def edgeTransformer[T](innerEdge: img.Graph[N, E]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
    val edge = innerEdge.outer
    Some(
      root,
      DotEdgeStmt(
        NodeId(edge.sources.head.toString),
        NodeId(edge.targets.head.toString)
      )
    )
  }
  private def cnodeTransformer[T](mapping: N => T, innerNode: img.Graph[N, E]#NodeT): Option[(DotGraph, DotNodeStmt)] = {
    val node = innerNode.outer
    Some(
      root,
      DotNodeStmt(node.toString, List(DotAttr(Id("label"), Id(mapping(node).toString))))
    )
  }

  def asDot[T](mapping: N => T): String = {
    val immutableGraph: img.Graph[N, E] = img.Graph.from(g.nodes.outerIterable, g.edges.outerIterable)
    immutableGraph.toDot(root, edgeTransformer, cNodeTransformer = Some(cnodeTransformer(mapping, _)))
  }
}
