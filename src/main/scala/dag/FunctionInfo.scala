package dag

import ast.{ConcreteFnDecl, LabelValue}
import common.FunctionIR
import scalax.collection.immutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, SemanticAnalysisInfo}
import tac.{Block, BlockEdge, Label}


case class FunctionInfo(functionDecl: ConcreteFnDecl,
                        returnSink: IRSymbol,
                        labelMap: Map[Label, Block],
                        labelSymbolMap: Map[LabelValue, Label],
                        startBlock: Label,
                        endBlock: Label,
                        symbolTable: FunctionSymbolTable,
                        flowGraph: Graph[Block, BlockEdge]
                       ) extends FunctionIR {

}

