package tac

import scalax.collection.edges.DiEdge
import scalax.collection.generic.{AbstractDiEdge, Edge}
import scalax.collection.immutable.Graph
import scalax.collection.io.dot.implicits.*
import scalax.collection.io.dot.*
import scalax.collection.{AnyGraph, GraphLike, immutable as img}
import tac.{Label, Tac}
import scalax.collection.edges.DiEdgeImplicits

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

class BlockEdge(source: Block, target: Block) extends AbstractDiEdge[Block](source, target) {

}

case class BlockTac(tac: Tac, block: Block) {
  override def toString: String = s"$tac in ${block.label}"
}

//type BlockEdge = DiEdge[Block]

case class Block(label: Label, tacs: List[Tac]) {

  override def toString: String = s"Block $label${tacs.mkString("{\n  ", "\n  ", "\n}")}"

  def self: Block = this

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
extension [CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]](g: GraphLike[Block, BlockEdge, CC]) {

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
