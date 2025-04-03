package semantic


case class Temp(id: Int) {
  override def toString: String = s"t$id"
}

object Temp {
  private var counter = 0
  def make: Temp = {
    counter += 1
    Temp(counter)
  }
}