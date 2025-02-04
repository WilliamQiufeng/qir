package ssa

import common.{CompilerContext, FunctionPass, FunctionPassResult}





case object RemoveRedundantBlockPass extends FunctionPass[SsaFunctionInfo, SsaFunctionInfo] {
  override def apply(in: SsaFunctionInfo)(implicit ctx: CompilerContext): FunctionPassResult[SsaFunctionInfo] = {

//    val remapping: Map[Label, Label] = FixedPoint.MapFixedPointState(Map.empty[Label, Label], false).iterateTillFixed(m =>
//
//      m.).value
    ???
  }
}
