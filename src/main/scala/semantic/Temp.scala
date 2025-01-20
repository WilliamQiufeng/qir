package semantic


case class Temp(id: Int = Temp.counter) {
  Temp.counter += 1
}

object Temp {
  private var counter = 0
}