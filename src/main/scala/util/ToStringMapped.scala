package util

trait ToStringMapped[A] {
  def toStringMapped[B](mapping: A => B): String
  override def toString: String = toStringMapped(x => x)
}
