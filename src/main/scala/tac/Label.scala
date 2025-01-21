package tac

case class Label(id: Int = Label.counter) {
  Label.counter += 1
}

object Label {
  private var counter = 0
}