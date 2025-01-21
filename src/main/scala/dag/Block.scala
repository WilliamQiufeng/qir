package dag

import scalax.collection.edges.DiEdge
import scalax.collection.generic.{AbstractDiEdge, Edge}
import scalax.collection.mutable.Graph
import tac.Tac

import scala.collection.mutable

class BlockEdge(source: Block, target: Block) extends AbstractDiEdge[Block](source, target) {

}

//type BlockEdge = DiEdge[Block]

class Block(val tacs: mutable.ArrayBuffer[Tac]) {
  override def toString = s"Block(${tacs.mkString("{\n  ", "\n  ", "\n}")})"
}

implicit class ExtGraph(val g: Graph[Block, BlockEdge]) {
  def foo: String = g.toString
}