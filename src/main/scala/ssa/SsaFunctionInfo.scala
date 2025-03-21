package ssa

import ast.{ConcreteFnDecl, LabelValue}
import common.FunctionIR
import dag.{FunctionHeader, FunctionInfo, WithFunctionInfo}
import scalax.collection.immutable.Graph
import semantic.{FunctionSymbolTable, SsaNormalSymbol, SsaSymbol, Temp}
import ssa.SsaGraph.GraphType
import tac.{Label, LabelEdge, asDot}

import scala.collection.mutable

case class SsaFunctionInfo(functionDecl: ConcreteFnDecl,
                           returnSink: Temp,
                           labelMap: Map[Label, SsaBlock],
                           labelSymbolMap: Map[LabelValue, Label],
                           startBlock: Label,
                           endBlock: Label,
                           symbolTable: FunctionSymbolTable,
                           flowGraph: Graph[Label, LabelEdge],
                           tempMap: Map[Temp, SsaSymbol],
                           header: FunctionHeader
                          ) extends FunctionIR, WithSsaFunctionInfo {
  def toDot: String = flowGraph.asDot(x => labelMap(x).toStringMapped(tempMap))

  lazy val defUse: Map[Temp, DefUse] = {
    val useChains = mutable.HashMap.empty[Temp, mutable.Set[SsaBlockTac]]
    val defs = mutable.HashMap.empty[Temp, SsaBlockTac]

    for block <- labelMap.values
        tac <- block.tacs
        use <- tac.sources do
      useChains.getOrElseUpdate(use, mutable.Set.empty[SsaBlockTac]).add(SsaBlockTac(tac, block.label))
      tac.definition.foreach(defs.update(_, SsaBlockTac(tac, block.label)))

    tempMap.keys.map(key =>
      key -> DefUse(defs.get(key), useChains.getOrElse(key, mutable.Set.empty).toList)
    ).toMap
  }

  lazy val ssaGraph: GraphType = SsaGraph.buildGraph(labelMap, defUse)

  def defUseToString: String = defUse.view.map((k, v) =>
    tempMap(k).toString + ": " + v
  ).mkString("DefUse:\n  ", "\n  ", "")

  override def functionInfo: SsaFunctionInfo = this
}
