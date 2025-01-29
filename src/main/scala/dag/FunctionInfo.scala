package dag

import ast.{ConcreteFnDecl, LabelValue}
import scalax.collection.mutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, SemanticAnalysis, SemanticAnalysisInfo}
import tac.{Block, BlockEdge, Label}

import scala.collection.mutable


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

