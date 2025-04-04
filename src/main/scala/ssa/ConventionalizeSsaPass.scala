package ssa

import common.{CompilerContext, FunctionPass, FunctionPassResult}
import semantic.{SsaDerivedSymbol, SsaSymbol, Temp}
import tac.{Label, ParallelCopy, Phi}

import scala.collection.mutable

case object ConventionalizeSsaPass extends FunctionPass[WithSsaFunctionInfo, WithSsaFunctionInfo] {
  override def apply(in: WithSsaFunctionInfo)(implicit ctx: CompilerContext): FunctionPassResult[WithSsaFunctionInfo] =
    val pcAfterPhi = mutable.Map.empty[Label, ParallelCopy].withDefaultValue(ParallelCopy(Map.empty))
    val pcEnd = mutable.Map.empty[Label, ParallelCopy].withDefaultValue(ParallelCopy(Map.empty))
    val phis = mutable.Map.empty[Label, List[Phi]].withDefaultValue(List())
    val addTempMap = mutable.Map.empty[Temp, SsaSymbol]
    for label <- in.functionInfo.flowGraph.nodes
        block = in.functionInfo.labelMap(label.outer)
    do
      for phi <- block.phis do
        val mapping = mutable.Map.empty[Temp, Temp]
        for (predLabel, a) <- phi.pairs do
          val aDef = in.functionInfo.defUse(a).definition
          // if the definition of a_i does not dominate PC_i then continue;
          if !aDef.exists { case SsaBlockTac(_, aDefLabel) => in.functionInfo.dominanceInfo.dominationMap.dom(aDefLabel, predLabel) } then
            val a2 = SsaDerivedSymbol(in.functionInfo.tempMap(a).origin)
            // Add a2 <- a to PC
            pcEnd.update(predLabel, pcEnd(predLabel).add(a2.temp, a))
            // Replace a with a2 in phi
            mapping.update(a, a2.temp)
            addTempMap.update(a2.temp, a2)

        // if one use of d is not dominated by PC_0 then continue;
        if !in.functionInfo.defUse(phi.definition).uses.exists { case SsaBlockTac(_, useLabel) => in.functionInfo.dominanceInfo.dominationMap.dom(label.outer, useLabel) } then
          val d2 = SsaDerivedSymbol(in.functionInfo.tempMap(phi.definition).origin)
          addTempMap.update(d2.temp, d2)
          pcAfterPhi.update(label.outer, pcAfterPhi(label.outer).add(phi.definition, d2.temp))
          mapping.update(phi.definition, d2.temp)
          phis.update(label.outer, phi.map(mapping).asInstanceOf[Phi] :: phis(label.outer))
          println(s"Mapping of $phi: $mapping")

    val newLabelMap = in.functionInfo.labelMap.map { case (label, b) =>
      label -> (b match
        case BasicSsaBlock(label, _, normalTacs, terminator) =>
          SsaBlockPc(label, phis(label), pcAfterPhi(label), normalTacs, pcEnd(label), terminator)
        case SsaBlockPc(label, _, oldPcAfterPhi, normalTacs, oldPcAtEnd, terminator) =>
          SsaBlockPc(label, phis(label), oldPcAfterPhi.merge(pcAfterPhi(label)), normalTacs, oldPcAtEnd.merge(pcEnd(label)), terminator)
        case _ => throw UnsupportedOperationException()
        )
    }
    println(newLabelMap)
    println(addTempMap)
    Right(in.updatedFunctionInfo(in.functionInfo.copy(labelMap = newLabelMap, tempMap = in.functionInfo.tempMap ++ addTempMap)))
}
