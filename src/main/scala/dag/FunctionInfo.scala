package dag

import ast.{ConcreteFnDecl, LabelValue}
import scalax.collection.immutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, SemanticAnalysisInfo}
import tac.{Block, BlockEdge, Label}


case class FunctionInfo(semanticAnalysis: SemanticAnalysisInfo,
                        functionDecl: ConcreteFnDecl,
                        returnSink: IRSymbol,
                        labelMap: Map[Label, Block],
                        labelSymbolMap: Map[LabelValue, Label],
                        flowGraph: Graph[Block, BlockEdge],
                        startBlock: Label,
                        endBlock: Label,
                        symbolTable: FunctionSymbolTable
                       ) {

}

