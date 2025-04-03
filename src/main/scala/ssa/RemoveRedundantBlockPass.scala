package ssa

import cats.implicits.toSemigroupKOps
import common.{CompilerContext, FunctionPass, FunctionPassResult}
import dag.FunctionDag
import semantic.ConstIRSymbol
import tac.{Branch, Goto, Label, Terminator}
import util.graph.Traversal.traverseDfsFold

case object RemoveRedundantBlockPass extends FunctionPass[WithSsaFunctionInfo, WithSsaFunctionInfo] {
  override def apply(in: WithSsaFunctionInfo)(implicit ctx: CompilerContext): FunctionPassResult[WithSsaFunctionInfo] = {
    val functionInfo = in.functionInfo
    val remapping = functionInfo.flowGraph.outerNodeDownUpTraverser(functionInfo.flowGraph get functionInfo.startBlock)
      .foldLeft[Map[Label, Terminator]](
        functionInfo.labelMap.map { case (label, b) => label -> b.terminator }
      ) {
        case (m, (down, l)) if !down =>
          val t = functionInfo.labelMap(l).terminator
          val next = t match
            case Branch(source, label1, label2) => functionInfo.tempMap(source) match {
              case ConstIRSymbol(const, _, _, _) => const.boolean match
                case Some(value) => if value then Some(label1) else Some(label2)
                case None => None
              case _ => None
            }
            case Goto(label) => Some(label)
            case _ => None
          m + (l -> (
            next match
              // l -> someNext -> possibly next of someNext
              case Some(someNext) if functionInfo.labelMap(someNext).isEmpty => m(someNext)
              case _ => t
            ))
        case (m, _) => m
      }

    val unprunedLabelMap = functionInfo.labelMap.map {
      case (label, block) => label -> block.rewriteTerminator(remapping(label))
    }

    val graph1 = FunctionDag.makeFlowGraph(unprunedLabelMap, functionInfo.startBlock)

    val prunedLabelMap = graph1.traverseDfsFold(graph1 get functionInfo.startBlock, Set.empty[Label]) {
      case (s, next) => s.incl(next)
    }.map(x => x -> unprunedLabelMap(x)).toMap


    Right(in.updatedFunctionInfo(
      functionInfo.copy(
        labelMap = prunedLabelMap,
        flowGraph = FunctionDag.makeFlowGraph(prunedLabelMap, functionInfo.startBlock)
      )
    ))
  }
}
