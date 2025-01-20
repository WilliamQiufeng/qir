package tac

import scalax.collection.generic.{AbstractDiEdge, Edge}
import scalax.collection.mutable.Graph

import scala.collection.mutable

class BlockEdge(source: Block, target: Block) extends AbstractDiEdge[Block](source, target) {

}

class Block(val tacs: mutable.ArrayBuffer[Tac]) {
}

implicit class ExtGraph(val g: Graph[Block, BlockEdge]) {
  def foo: String = g.toString
}