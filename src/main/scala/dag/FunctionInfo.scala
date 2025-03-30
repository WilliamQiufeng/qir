package dag

import ast.{ConcreteFnDecl, LabelValue}
import common.FunctionIR
import scalax.collection.immutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, Temp}
import tac.{Label, LabelEdge, NormalBlock}
import util.graph.Dominance.DominanceInfo

import util.graph.Dominance.makeDominanceInfo

case class FunctionInfo(functionDecl: ConcreteFnDecl,
                        returnSink: Temp,
                        labelMap: Map[Label, NormalBlock],
                        labelSymbolMap: Map[LabelValue, Label],
                        startBlock: Label,
                        endBlock: Label,
                        symbolTable: FunctionSymbolTable,
                        flowGraph: Graph[Label, LabelEdge],
                        tempMap: Map[Temp, IRSymbol],
                        header: FunctionHeader
                       ) extends FunctionIR, WithFunctionInfo {
  lazy val dominanceInfo: DominanceInfo[Label] = flowGraph.makeDominanceInfo(startBlock)
  override def functionInfo: FunctionInfo = this
}

