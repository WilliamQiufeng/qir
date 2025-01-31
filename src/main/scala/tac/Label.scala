package tac

import scalax.collection.generic.AbstractDiEdge

case class Label(id: Int = Label.counter) {
  Label.counter += 1

  override def toString: String = s"L$id"
}

object Label {
  private var counter = 0
}

class LabelEdge(source: Label, target: Label) extends AbstractDiEdge[Label](source, target) {

}