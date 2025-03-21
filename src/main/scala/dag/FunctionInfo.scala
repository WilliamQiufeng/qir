package dag

import ast.{ConcreteFnDecl, LabelValue}
import common.FunctionIR
import scalax.collection.immutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, Temp}
import tac.{Block, Label, LabelEdge}


case class FunctionInfo(functionDecl: ConcreteFnDecl,
                        returnSink: Temp,
                        labelMap: Map[Label, Block],
                        labelSymbolMap: Map[LabelValue, Label],
                        startBlock: Label,
                        endBlock: Label,
                        symbolTable: FunctionSymbolTable,
                        flowGraph: Graph[Label, LabelEdge],
                        tempMap: Map[Temp, IRSymbol],
                        header: FunctionHeader
                       ) extends FunctionIR, WithFunctionInfo {

  override def functionInfo: FunctionInfo = this
}

