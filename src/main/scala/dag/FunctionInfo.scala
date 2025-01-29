package dag

import ast.{ConcreteFnDecl, LabelValue}
import scalax.collection.mutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, SemanticAnalysis}
import tac.Label

import scala.collection.mutable


case class FunctionInfo(semanticAnalysis: SemanticAnalysis,
                        functionDecl: ConcreteFnDecl,
                        returnSink: IRSymbol,
                        labelMap: mutable.HashMap[Label, Block],
                        labelSymbolMap: mutable.HashMap[LabelValue, Label],
                        flowGraph: Graph[Block, BlockEdge],
                        startBlock: Label,
                        endBlock: Label,
                        symbolTable: FunctionSymbolTable
                       ) {

}

