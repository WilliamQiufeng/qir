package util

import algebra.lattice.{JoinSemilattice, MeetSemilattice}

package object syntax {
  object LatticeSyntax {
    implicit class MeetOps[T, CC <: MeetSemilattice[T]](val self: T) extends AnyVal {
      // Define the infix meet operator '^'
      def ^(other: T)(implicit lattice: CC): T =
        lattice.meet(self, other)
    }

    implicit class JoinOps[T, CC <: JoinSemilattice[T]](val self: T) extends AnyVal {
      // Define the infix meet operator '^'
      def |(other: T)(implicit lattice: CC): T =
        lattice.join(self, other)
    }
  }
}
