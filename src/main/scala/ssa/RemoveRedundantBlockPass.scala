package ssa

import cats.implicits.toSemigroupKOps
import common.{CompilerContext, FunctionPass, FunctionPassResult}
import dag.FunctionDag
import semantic.ConstIRSymbol
import tac.{Branch, Goto, Label}
import util.graph.FixedPoint
import util.graph.Traversal.traverseDfsFold

case object RemoveRedundantBlockPass extends FunctionPass[WithSsaFunctionInfo, WithSsaFunctionInfo] {
  override def apply(in: WithSsaFunctionInfo)(implicit ctx: CompilerContext): FunctionPassResult[WithSsaFunctionInfo] = {
    val functionInfo = in.functionInfo
    // TODO fix cycling
    val remapping = FixedPoint.MapFixedPointState[Label, Option[Label]](functionInfo.labelMap.map { case (label, _) => label -> None }, false).iterateTillFixed(m =>
      val x = for (l, next) <- m.value yield
        //          if functionInfo.labelMap(l).tacs.size != 1 then
        //            l -> None
        //          else
        l -> (
          next match
            // l -> someNext -> possibly next of someNext
            case Some(someNext) => if functionInfo.labelMap(someNext).isEmpty then m.value(someNext) <+> next else next
            case None =>
              val t = functionInfo.labelMap(l).terminator
              t match
                case Branch(source, label1, label2) => functionInfo.tempMap(source) match {
                  case ConstIRSymbol(const, _, _, _) => const.boolean match
                    case Some(value) => if value then Some(label1) else Some(label2)
                    case None => None
                  case _ => None
                }
                case Goto(label) => Some(label)
                case _ => None
          )
      println(x)
      m << x
    ).value

    val unprunedLabelMap = functionInfo.labelMap.map {
      case (label, block) => label -> remapping(label).map(next => block.rewriteTerminator(Goto(next))).getOrElse(block)
    }

    val graph1 = FunctionDag.makeFlowGraph(
      unprunedLabelMap,
      functionInfo.startBlock,
      Some(unprunedLabelMap(functionInfo.startBlock).terminator.targets(0))
    )

    val prunedLabelMap = graph1.traverseDfsFold(graph1 get functionInfo.startBlock, Set.empty[Label]){
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
