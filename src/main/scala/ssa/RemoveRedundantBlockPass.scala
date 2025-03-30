package ssa

import cats.implicits.toSemigroupKOps
import common.{CompilerContext, FunctionPass, FunctionPassResult}
import dag.FunctionDag
import semantic.ConstIRSymbol
import tac.{Branch, Goto, Label}
import util.graph.Traversal.traverseDfsFold

case object RemoveRedundantBlockPass extends FunctionPass[WithSsaFunctionInfo, WithSsaFunctionInfo] {
  override def apply(in: WithSsaFunctionInfo)(implicit ctx: CompilerContext): FunctionPassResult[WithSsaFunctionInfo] = {
    val functionInfo = in.functionInfo
    // TODO fix cycling
    val remapping = functionInfo.flowGraph.outerNodeDownUpTraverser(functionInfo.flowGraph get functionInfo.startBlock)
      .foldLeft[Map[Label, Option[Label]]](
        functionInfo.labelMap.map { case (label, _) => label -> None }
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
              case Some(someNext) => if functionInfo.labelMap(someNext).isEmpty then m(someNext) <+> next else next
              case None => next
            ))
        case (m, _) => m
      }

    val unprunedLabelMap = functionInfo.labelMap.map {
      case (label, block) => label -> remapping(label).map(next => block.rewriteTerminator(Goto(next))).getOrElse(block)
    }

    val graph1 = FunctionDag.makeFlowGraph(
      unprunedLabelMap,
      functionInfo.startBlock,
      Some(unprunedLabelMap(functionInfo.startBlock).terminator.targets(0))
    )

    val prunedLabelMap = graph1.traverseDfsFold(graph1 get functionInfo.startBlock, Set.empty[Label]) {
      case (s, next) => s.incl(next)
    }.map(x => x -> unprunedLabelMap(x)).toMap


    Right(in.updatedFunctionInfo(
      functionInfo.copy(
        labelMap = prunedLabelMap,
        flowGraph = FunctionDag.makeFlowGraph(
          prunedLabelMap,
          functionInfo.startBlock,
          Some(prunedLabelMap(functionInfo.startBlock).terminator.targets(0))
        )
      )
    ))
  }
}
