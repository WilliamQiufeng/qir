package common

import cats.implicits.catsSyntaxFlatMapOps
import semantic.SemanticAnalysisInfo

trait FunctionIR

case class CompilerContext(semanticAnalysisInfo: SemanticAnalysisInfo) {
}

type FunctionPassResult[A] = Either[IRError, A]

trait FunctionPass[-In <: FunctionIR, +Out <: FunctionIR] {
  def apply(in: In)(implicit ctx: CompilerContext): FunctionPassResult[Out]
}

implicit class FunctionPassOps[In <: FunctionIR, Mid <: FunctionIR](pass: FunctionPass[In, Mid]) {
  def andThen[Out <: FunctionIR](next: FunctionPass[Mid, Out]): FunctionPass[In, Out] = new FunctionPass[In, Out] {
    def apply(in: In)(implicit ctx: CompilerContext): FunctionPassResult[Out] = pass(in) >>= next.apply
  }
}