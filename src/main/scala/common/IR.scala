package common

sealed trait IR

trait CompilerContext

trait Pass[-In <: IR, +Out <: IR] {
  def apply(in: In)(implicit ctx: CompilerContext): Out
}

implicit class PassOps[In <: IR, Mid <: IR](pass: Pass[In, Mid]) {
  def andThen[Out <: IR](next: Pass[Mid, Out]): Pass[In, Out] = new Pass[In, Out] {
    def apply(in: In)(implicit ctx: CompilerContext): Out = next(pass(in))
  }
}