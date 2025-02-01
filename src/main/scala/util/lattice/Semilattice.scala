package util.lattice

import scala.annotation.targetName

trait Meetable[+T <: Meetable[T]] {
  this: T =>
  def meet[S >: T <: Meetable[S]](t: S): Semilattice[T]

}

sealed trait Semilattice[+T <: Meetable[T]] { this: Semilattice[T] =>

  @targetName("meetOp")
  def ^[S >: T <: Meetable[S]](that: Semilattice[S]): Semilattice[S] = {
    if this == Bottom || that == Bottom then
      return Bottom
    if this == Top then
      return that
    if that == Top then
      return this
    val na = this.asInstanceOf[Normal[T]]
    val nb = that.asInstanceOf[Normal[S]]
    na.value.meet(nb.value)
  }
}

extension [T <: Meetable[T]] (a: Semilattice[T]) {
  def >=(b: Semilattice[T]): Boolean = (a ^ b) == b
  inline def <(b: Semilattice[T]): Boolean = !(a >= b)
  inline def <=(b: Semilattice[T]): Boolean = b >= a
  inline def >(b: Semilattice[T]): Boolean = !(a <= b)
}


case class Normal[T <: Meetable[T]](value: T) extends Semilattice[T]

case object Bottom extends Semilattice[Nothing]

case object Top extends Semilattice[Nothing]