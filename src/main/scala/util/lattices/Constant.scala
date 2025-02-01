package util.lattices

import algebra.lattice.BoundedLattice
import cats.Eq
import util.syntax.LatticeSyntax.{JoinOps, MeetOps}

sealed trait ConstantLattice[+T]

case object ConstantBottom extends ConstantLattice[Nothing]

case object ConstantTop extends ConstantLattice[Nothing]

case class ConstantValue[T](value: T) extends ConstantLattice[T]

import algebra.lattice.BoundedLattice

implicit def ceq[T]: Eq[T] = (x: T, y: T) => x == y

implicit def constantBoundedLattice[T]: BoundedLattice[ConstantLattice[T]] = new BoundedLattice[ConstantLattice[T]] {
  def join(a: ConstantLattice[T], b: ConstantLattice[T]): ConstantLattice[T] = (a, b) match {
    case (ConstantBottom, x) => x
    case (x, ConstantBottom) => x
    case (ConstantTop, _) => ConstantTop
    case (_, ConstantTop) => ConstantTop
    case (ConstantValue(x), ConstantValue(y)) =>
      if (x == y) ConstantValue(x) else ConstantTop
  }

  def meet(a: ConstantLattice[T], b: ConstantLattice[T]): ConstantLattice[T] = (a, b) match {
    case (ConstantTop, x) => x
    case (x, ConstantTop) => x
    case (ConstantBottom, _) => ConstantBottom
    case (_, ConstantBottom) => ConstantBottom
    case (ConstantValue(x), ConstantValue(y)) =>
      if (x == y) ConstantValue(x) else ConstantBottom
  }

  def zero: ConstantLattice[T] = ConstantBottom

  def one: ConstantLattice[T] = ConstantTop
}