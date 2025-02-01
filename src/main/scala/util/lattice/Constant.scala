package util.lattice

case class Constant[T](value: T) extends Meetable[Constant[T]] {
  override def meet[S >: Constant[T] <: Meetable[S]](t: S): Semilattice[Constant[T]] = t match {
    case Constant(x) if value == x => Normal(this)
    case _ => Bottom
  }
}