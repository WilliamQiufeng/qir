package common

import cats.implicits.catsSyntaxFlatMapOps

trait IR

trait CompilerContext

type PassResult[A] = Either[IRError, A]

trait Pass[-In <: IR, +Out <: IR] {
  def apply(in: In)(implicit ctx: CompilerContext): PassResult[Out]
}

implicit class PassOps[In <: IR, Mid <: IR](pass: Pass[In, Mid]) {
  def andThen[Out <: IR](next: Pass[Mid, Out]): Pass[In, Out] = new Pass[In, Out] {
    def apply(in: In)(implicit ctx: CompilerContext): PassResult[Out] = pass(in) >>= next.apply
  }
}