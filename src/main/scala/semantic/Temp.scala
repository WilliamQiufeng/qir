package semantic


case class Temp(id: Int = Temp.counter) {
  Temp.counter += 1

  override def toString: String = s"t$id"
}

object Temp {
  private var counter = 0
  def make: Temp = Temp()
}