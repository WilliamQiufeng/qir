package util.lattice

trait PartiallyOrdered[+T <: PartiallyOrdered[T]] {
  this: T =>
  def tryCompareTo[S >: T](that: S): Option[Int]

  def >=[S >: T](that: S): Boolean = tryCompareTo(that) match {
    case Some(x) if x >= 0 => true
    case _ => false
  }

  def <=[S >: T](that: S): Boolean = tryCompareTo(that) match {
    case Some(x) if x <= 0 => true
    case _ => false
  }

  inline def <[S >: T](that: S): Boolean = !(this >= that)

  inline def >[S >: T](that: S): Boolean = !(this <= that)
}
