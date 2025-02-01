package util.lattices

import algebra.lattice.{BoundedMeetSemilattice, Lattice}

trait UpperBoundedLattice[A] extends BoundedMeetSemilattice[A] with Lattice[A]

implicit def setBoundedLattice[T]: UpperBoundedLattice[Set[T]] = new UpperBoundedLattice[Set[T]] {

  def meet(a: Set[T], b: Set[T]): Set[T] = a union b

  def join(a: Set[T], b: Set[T]): Set[T] = a intersect b

  def one: Set[T] = Set.empty
}