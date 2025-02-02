package ssa

import ast.{ConcreteFnDecl, LabelValue}
import common.FunctionIR
import scalax.collection.immutable.Graph
import semantic.{FunctionSymbolTable, SsaSymbol, Temp}
import tac.{Label, LabelEdge, asDot}

case class SsaFunctionInfo(functionDecl: ConcreteFnDecl,
                           returnSink: Temp,
                           labelMap: Map[Label, SsaBlock],
                           labelSymbolMap: Map[LabelValue, Label],
                           startBlock: Label,
                           endBlock: Label,
                           symbolTable: FunctionSymbolTable,
                           flowGraph: Graph[Label, LabelEdge],
                           tempMap: Map[Temp, SsaSymbol],
                           defUse: Map[Temp, DefUse]
                          ) extends FunctionIR {
  def toDot: String = flowGraph.asDot(x => labelMap(x).toStringMapped(tempMap))

  def defUseToString: String = defUse.view.map((k, v) =>
    tempMap(k).toString + ": " + v
  ).mkString("DefUse:\n  ", "\n  ", "")
}
