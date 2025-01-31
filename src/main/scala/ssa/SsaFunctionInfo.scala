package ssa
import ast.{ConcreteFnDecl, LabelValue}
import common.FunctionIR
import scalax.collection.immutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, SsaSymbol, Temp}
import tac.{Block, Label, LabelEdge}

case class SsaFunctionInfo(functionDecl: ConcreteFnDecl,
                           returnSink: Temp,
                           labelMap: Map[Label, SsaBlock],
                           labelSymbolMap: Map[LabelValue, Label],
                           startBlock: Label,
                           endBlock: Label,
                           symbolTable: FunctionSymbolTable,
                           flowGraph: Graph[Label, LabelEdge],
                           tempMap: Map[Temp, SsaSymbol]
                          ) extends FunctionIR {
  
}
