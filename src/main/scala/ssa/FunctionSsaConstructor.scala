package ssa

import cats.data.State
import cats.implicits.*
import common.{CompilerContext, FunctionPass, FunctionPassResult}
import dag.*
import scalax.collection.immutable.Graph as ImmutableGraph
import semantic.*
import tac.*
import util.graph.Dominance.*

import scala.collection.mutable

case object SsaConstructionPass extends FunctionPass[FunctionInfo, SsaFunctionInfo] {
  override def apply(in: FunctionInfo)(implicit ctx: CompilerContext): FunctionPassResult[SsaFunctionInfo] =
    FunctionSsaConstructor(in).perform.pure
}

case class FunctionSsaConstructor(functionInfo: FunctionInfo) extends WithFunctionInfo {
  private val dominanceInfo = functionInfo.flowGraph.makeDominanceInfo(functionInfo.startBlock)
  private val globals: Set[Temp] = {
    var res: Set[Temp] = Set.empty
    for b <- functionInfo.flowGraph.nodes.outerIterator.map(functionInfo.labelMap) do
      val varKill: mutable.Set[Temp] = mutable.Set.empty
      for i <- b.tacs do
        res = res | i.sources.filterNot(varKill.contains).toSet
    res
  }
  private val globalBlocks: Map[Temp, Set[Label]] = {
    val res = mutable.HashMap.empty[Temp, Set[Label]]
    for b <- functionInfo.flowGraph.nodes.outerIterator.map(functionInfo.labelMap) do
      val varKill: mutable.Set[Temp] = mutable.Set.empty
      for i <- b.tacs do
        i.definition.foreach(dst =>
          varKill.addOne(dst)
          res.updateWith(dst) {
            case None => Some(Set.empty)
            case Some(s) => Some(s + b.label)
          }
        )
    res.toMap
  }

  private def insertPhi(): mutable.Map[Label, SsaBlock] = {
    val res = mutable.HashMap.from(functionInfo.labelMap.map((k, v) => k -> SsaBlock(v.label, List(), v.tacs.toIndexedSeq)))
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
            res.updateWith(dfLabel) {
              case None => throw RuntimeException()
              case Some(df) => Some(df.copy(phis = Tac(sources, x.some, Phi(preds)) :: df.phis))
            }
            workList += dfLabel
    res
  }

  private def rename(blockLabel: Label, labelMap: mutable.Map[Label, SsaBlock], newTempMap: mutable.Map[Temp, SsaSymbol] = mutable.Map.empty): Unit = {
    val newSymbolStack = mutable.Map.empty[Temp, mutable.Stack[Temp]]

    def getStack(temp: Temp): mutable.Stack[Temp] = newSymbolStack.getOrElseUpdate(temp, mutable.Stack.empty)

    def renameInternal(blockLabel: Label): Unit = {
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
            phis = block.phis.map(phi => phi.copy(definition = newName(phi.definition.get).temp.some)),
            trailingTacs = block.trailingTacs.map { i =>
              i.copy(
                sources = i.sources.map { src =>
                  if globals.contains(src) && getStack(src).nonEmpty then
                    getStack(src).top
                  else
                    src
                },
                definition = i.definition.map { definition =>
                  if globals.contains(definition) then newName(definition).temp else definition
                })
            }).some
        case None => None
      }
      (functionInfo.flowGraph get blockLabel).diSuccessors.foreach(l =>
        labelMap.updateWith(l) {
          case Some(block) => block.copy(
            phis = block.phis.map { tac =>
              tac.impl.replace(blockLabel, src => if getStack(src).nonEmpty then getStack(src).top else src, tac)
            }).some
          case None => None
        })

      for s <- (dominanceInfo.dominationTree get blockLabel).diSuccessors do
        renameInternal(s)

      symbolsOverwritten.foreach(s => getStack(s.temp).pop())
    }

    renameInternal(blockLabel)
  }

  def perform: SsaFunctionInfo = {
    val labelMap = insertPhi()
    val newTempMap: mutable.Map[Temp, SsaSymbol] = mutable.Map.empty

    rename(functionInfo.startBlock, labelMap, newTempMap)
    for (k, v) <- functionInfo.tempMap do
      if !newTempMap.contains(k) then
        newTempMap += k -> SsaNormalSymbol(v.temp, v.ty, v.debugName, v.undefined)

    //    functionInfo.flowGraph.nodes.foreach(_.fillDefUse())
    SsaFunctionInfo(functionInfo.functionDecl, functionInfo.returnSink, labelMap.toMap, functionInfo.labelSymbolMap,
      functionInfo.startBlock, functionInfo.endBlock, functionInfo.symbolTable, functionInfo.flowGraph, newTempMap.toMap)
  }

}
