package ssa

import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxOptionId}
import common.{CompilerContext, FunctionPass, FunctionPassResult}
import dag.*
import semantic.*
import tac.*
import util.graph.Dominance.*

import scala.collection.mutable

case object SsaConstructionPass extends FunctionPass[FunctionInfo, SsaFunctionInfo] {
  override def apply(in: FunctionInfo)(implicit ctx: CompilerContext): FunctionPassResult[SsaFunctionInfo] =
    FunctionSsaConstructor(in).result.pure
}

case class FunctionSsaConstructor(functionInfo: FunctionInfo) extends WithFunctionInfo {
  private val dominanceInfo = functionInfo.flowGraph.makeDominanceInfo(functionInfo.startBlock)
  private val globals: Set[Temp] = {
    var res: Set[Temp] = Set.empty
    for b <- functionInfo.flowGraph.nodes.outerIterator.map(functionInfo.labelMap) do
      val varKill: mutable.Set[Temp] = mutable.Set.empty
      for i <- b.tacs do
        res = res | i.sources.filterNot(varKill.contains).toSet
        varKill.addAll(i.definitions)

    res
  }
  private val globalBlocks: Map[Temp, Set[Label]] = {
    val res = mutable.HashMap.empty[Temp, Set[Label]].withDefaultValue(Set.empty)
    for b <- functionInfo.flowGraph.nodes.outerIterator.map(functionInfo.labelMap) do
      for i <- b.tacs do
        i.definitions.foreach(dst => res.update(dst, res(dst) + b.label))
    res.toMap
  }
  private val labelMap: mutable.Map[Label, BasicSsaBlock] = mutable.HashMap.from(
    functionInfo.labelMap.map((k, v) =>
      k -> BasicSsaBlock(v.label, List(), v.normalTacs.toIndexedSeq, v.terminator)
    )
  )
  private val newTempMap: mutable.Map[Temp, SsaSymbol] = mutable.Map.empty
  private val newSymbolStack = mutable.Map.empty[Temp, mutable.Stack[Temp]]

  private def getStack(temp: Temp): mutable.Stack[Temp] = newSymbolStack.getOrElseUpdate(temp, mutable.Stack.empty)

  insertPhi()

  rename(functionInfo.startBlock)
  for (k, v) <- functionInfo.tempMap do
    if !newTempMap.contains(k) then
      newTempMap +=
        k -> (v match
          case c: ConstIRSymbol[_] => c
          case _ => SsaNormalSymbol(v.temp, v.ty, v.debugName, v.undefined)
          )

  val result: SsaFunctionInfo = {
    val newLabelMap = labelMap.toMap
    SsaFunctionInfo(functionInfo.functionDecl, findNewReturnSink, newLabelMap, functionInfo.labelSymbolMap,
      functionInfo.startBlock, functionInfo.endBlock, functionInfo.symbolTable, functionInfo.flowGraph, newTempMap.toMap,
      functionInfo.header)
  }

  private def insertPhi(): Unit = {
    for x <- globals do
      val insertedBlocks: mutable.Set[Label] = mutable.Set.empty
      val workList: mutable.Set[Label] = mutable.Set.from(globalBlocks.getOrElse(x, Set.empty))
      while workList.nonEmpty do
        val b = workList.head
        workList -= b
        for dfLabel <- dominanceInfo.dominanceFrontierMap(b) do
          if insertedBlocks.add(dfLabel) then
            val preds = (functionInfo.flowGraph get dfLabel).diPredecessors.map(_.outer).toIndexedSeq
            val sources = preds.map(_ => x)
            labelMap.updateWith(dfLabel) {
              case None => throw RuntimeException()
              case Some(df) => Some(df.copy(phis = Phi(x, sources, preds) :: df.phis))
            }
            workList += dfLabel
  }

  private def rename(blockLabel: Label): Unit = {
    val symbolsOverwritten: mutable.Set[IRSymbol] = mutable.Set.empty

    // We don't need to keep track of more than one rewritten variables
    def newName(temp: Temp): SsaDerivedSymbol = {
      val symbol = newTempMap.getOrElseUpdate(temp, {
        val sym = functionInfo.tempMap(temp)
        SsaNormalSymbol(sym.temp, sym.ty, sym.debugName, sym.undefined)
      })
      val newSym = SsaDerivedSymbol(symbol.origin)
      newTempMap += newSym.temp -> newSym
      if !symbolsOverwritten.add(symbol) then
        getStack(symbol.temp).pop()
      getStack(symbol.temp).push(newSym.temp)
      newSym
    }

    // Give phi targets a new name
    labelMap.updateWith(blockLabel) {
      case Some(block) =>
        block.copy(
          phis = block.phis.map(phi => phi.copy(definition = newName(phi.definition).temp)),
          normalTacs = block.normalTacs.map { i =>
            val newSources = rewriteSources(i)
            val newDefinitions = i.definitions.map { definition =>
              newName(definition).temp
            }
            i.rewrite(newDefinitions, newSources)
          },
          terminator = block.terminator.rewrite(block.terminator.definitions, rewriteSources(block.terminator))
        ).some
      case None => None
    }
    (functionInfo.flowGraph get blockLabel).diSuccessors.foreach(l =>
      labelMap.updateWith(l) {
        case Some(block) => block.copy(
          phis = block.phis.map { tac =>
            tac.replace(blockLabel, src => if getStack(src).nonEmpty then getStack(src).top else src, tac)
          }).some
        case None => None
      })

    for s <- (dominanceInfo.dominationTree get blockLabel).diSuccessors do
      rename(s)

    symbolsOverwritten.foreach(s => getStack(s.temp).pop())
  }

  private def rewriteSources(i: Tac) = {
    i.sources.map { src =>
      if getStack(src).nonEmpty then
        getStack(src).top
      else
        src
    }
  }

  private def findNewReturnSink: Temp = labelMap(functionInfo.endBlock).terminator match
    case Ret(src) => src
    case _ => throw RuntimeException()
}
