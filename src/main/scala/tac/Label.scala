package tac

case class Label(id: Int = Label.counter) {
  Label.counter += 1

  override def toString: String = s"L$id"
}

object Label {
  private var counter = 0
}