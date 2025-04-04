package ssa

import cats.implicits.toSemigroupKOps
import common.{CompilerContext, FunctionPass, FunctionPassResult}
import scalax.collection.immutable.Graph

import semantic.{ConstIRSymbol, SsaSymbol, Temp}
import ssa.SsaGraph.GraphType
import tac.*
import util.lattices.*
import util.syntax.LatticeSyntax.MeetOps

import scala.collection.mutable

case object SCCPPass extends FunctionPass[WithSsaFunctionInfo, SsaFunctionInfo] {
  private def replace[T <: Tac](t: T, mapping: Map[Temp, ConstantLattice[semantic.Const[?]]], constMap: Map[semantic.Const[?], Temp]): Option[T] = {
    if t.definitions.flatMap(mapping.get).exists(_ match
      case ConstantValue(value) => true
      case _ => false)
    then return None
    val sources: IndexedSeq[Temp] = t.sources.map(src => (
      (
        mapping.get(src) match
          case Some(ConstantValue(value)) => Some(constMap(value))
          case _ => None
        )
        <+> Some(src)
      ).getOrElse(src))
    Some(t.rewrite(t.definitions, sources).asInstanceOf[T])
  }

  override def apply(in: WithSsaFunctionInfo)(implicit ctx: CompilerContext): FunctionPassResult[SsaFunctionInfo] = {
    val functionInfo = in.functionInfo
    val result = SparseConditionalConstantPropagation(functionInfo).result
    var newSymbolTable = functionInfo.symbolTable
    val newTempMapMut = mutable.Map.from(functionInfo.tempMap)
    val constMapMut = mutable.Map.empty[semantic.Const[?], Temp]
    for (_, lattice) <- result.value do
      lattice match
        case ConstantValue(value) =>
          val temp = Temp.make
          newTempMapMut += (temp -> ConstIRSymbol(value, temp, value.ty))
          constMapMut += value -> temp
        case _ => ()
    val newTempMap = newTempMapMut.toMap
    val constMap = constMapMut.toMap

    val newLabelMap = for (label, block) <- functionInfo.labelMap yield
      label -> block.rewrite(
        label,
        block.phis.flatMap(replace(_, result.value, constMap)),
        block.trailingTacs.flatMap(replace(_, result.value, constMap)).toIndexedSeq
      )

    val newGraph = Graph.from(functionInfo.flowGraph.nodes.outerIterable, result.executable)
    Right(functionInfo.copy(
      flowGraph = newGraph,
      tempMap = newTempMap,
      labelMap = newLabelMap,
    ))
  }
}

case class SparseConditionalConstantPropagation(ssaFunctionInfo: SsaFunctionInfo) {
  private val ssaGraph: GraphType = ssaFunctionInfo.ssaGraph
  private val ssaWorklist: mutable.Queue[ssaGraph.EdgeT] = mutable.Queue.empty[ssaGraph.EdgeT]
  private val cfgWorklist = mutable.Queue.from(ssaFunctionInfo.flowGraph.get(ssaFunctionInfo.startBlock).edges)
  private val value = mutable.Map.empty[Temp, ConstantLattice[semantic.Const[?]]]
  private val executable = mutable.Set.empty[ssaFunctionInfo.flowGraph.EdgeT]
  run()
  val result: Result = Result(value.toMap, executable.map(edge => LabelEdge(edge.source, edge.target)).toSet)

  case class Result(value: Map[Temp, ConstantLattice[semantic.Const[?]]], executable: Set[LabelEdge])

  private def valueOf(temp: Temp): ConstantLattice[semantic.Const[?]] = {
    value.getOrElseUpdate(temp, ConstantTop)
  }

  private def evaluateBranch(t: Tac, blockLabel: Label) = {
    t match
      case jump: Terminator => jump match
        case Goto(label) => cfgWorklist += ssaFunctionInfo.flowGraph.get(LabelEdge(blockLabel, label))
        case Branch(cond, label1, label2) =>
          val edge1 = ssaFunctionInfo.flowGraph.get(LabelEdge(blockLabel, label1))
          val edge2 = ssaFunctionInfo.flowGraph.get(LabelEdge(blockLabel, label2))
          valueOf(cond) match
            case ConstantBottom =>
              cfgWorklist += edge1
              cfgWorklist += edge2
            case ConstantValue(value: semantic.Const[?]) =>
              cfgWorklist += (if value.boolean.exists{x => x} then edge1 else edge2)
            case _ => ()
        case Ret(_) => ()
      case _ => throw new Exception("This should not happen")
  }

  private def evaluate(t: Tac, blockLabel: Label): ConstantLattice[semantic.Const[?]] = {
    t match
      case _: tac.Terminator => throw new Exception("Jump shouldn't be handled here")
      case tac.BinaryArith(_, l, r, op) =>
        val left = valueOf(l)
        val right = valueOf(r)
        op match
          case BinaryArithOp.AddI => left.combine(right)((_, _) match
            case (semantic.ConstInt(a, _), semantic.ConstInt(b, _)) => semantic.ConstInt(a + b)
            case _ => throw new Exception("Type mismatch")
          )
          case BinaryArithOp.SubI => left.combine(right)((_, _) match
            case (semantic.ConstInt(a, _), semantic.ConstInt(b, _)) => semantic.ConstInt(a - b)
            case _ => throw new Exception("Type mismatch")
          )
          case BinaryArithOp.MulI if left == semantic.ConstInt(0) || right == semantic.ConstInt(0) =>
            ConstantValue(semantic.ConstInt(0))
          case BinaryArithOp.MulI => left.combine(right)((_, _) match
            case (semantic.ConstInt(a, _), semantic.ConstInt(b, _)) => semantic.ConstInt(a * b)
            case _ => throw new Exception("Type mismatch")
          )
          case BinaryArithOp.DivI => left.combine(right)((_, _) match
            case (semantic.ConstInt(a, _), semantic.ConstInt(b, _)) => semantic.ConstInt(a / b)
            case _ => throw new Exception("Type mismatch")
          )
      case tac.Move(_, src) => valueOf(src)
      case tac.Call(_, _, fnName) => ConstantBottom
      case tac.Phi(_, _, _) => throw new Exception(s"Phi shouldn't appear here")
  }

  private def visitExpression(t: Tac, blockLabel: Label): Unit = {
    t match
      case _: tac.Terminator => evaluateBranch(t, blockLabel)
      case _ =>
        val right = evaluate(t, blockLabel)
        t.definitions.foreach(definition =>
          ssaWorklist.enqueueAll(ssaGraph.get(SsaBlockTac(t, blockLabel)).outgoing)
          value.updateWith(definition) {
            case Some(value) => Some(value ^ right)
            case None => Some(right)
          })
  }

  private def visitPhi(phi: Phi, blockLabel: Label): Unit = {
    // Update the value of definition. If the block of source is not marked, use top
    value.update(phi.definition,
      phi.blockLabels
        .zip(phi.sources)
        .map((srcLabel, srcTemp) =>
          if executable.contains(ssaFunctionInfo.flowGraph get LabelEdge(srcLabel, blockLabel))
          then valueOf(srcTemp)
          else ConstantTop)
        .foldLeft(ConstantTop)(constantBoundedLattice[semantic.Const[?]].meet))
  }

  def run(): Unit = {

    for (temp, sym) <- ssaFunctionInfo.tempMap do
      value += temp -> (sym match
        case const: ConstIRSymbol[?] => ConstantValue(const.const)
        case _ => if sym.undefined then ConstantBottom else ConstantTop
        )

    while ssaWorklist.nonEmpty || cfgWorklist.nonEmpty do
      while cfgWorklist.nonEmpty do
        val b = cfgWorklist.dequeue()
        if !executable.contains(b) then
          executable += b
          val destBlock = ssaFunctionInfo.labelMap(b.target)
          // Evaluate each phi
          for phi <- destBlock.phis do
            visitPhi(phi, destBlock.label)

          // If only one of the ExecutableFlags of incoming flow graph
          // edges for dest is true(dest visited for the first time),then
          // VisitExpression for all expressions in dest
          if ssaFunctionInfo.flowGraph.get(destBlock.label).incoming.count(executable.contains) == 1 then
            // Evaluate each trailing stmts
            for t <- destBlock.trailingTacs do
              visitExpression(t, destBlock.label)

            // If the dest contains only one outgoing flow graph edge,add that edge to FWL
            val destOutgoingEdges = ssaFunctionInfo.flowGraph.get(destBlock.label).outgoing
            if destOutgoingEdges.size == 1 then
              cfgWorklist.enqueue(destOutgoingEdges.head)

      while ssaWorklist.nonEmpty do
        val edge = ssaWorklist.dequeue()
        val dest = edge.target
        dest.tac match
          case phi: Phi => visitPhi(phi, dest.label)
          case _ if ssaFunctionInfo.flowGraph.get(dest.label).incoming.exists(executable.contains) =>
            visitExpression(dest.tac, dest.label)

          case _ => ()
  }
}
