package ssa

import ast.{ConcreteFnDecl, LabelValue}
import dag.{Block, BlockEdge}
import scalax.collection.mutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, SemanticAnalysis, Temp}
import tac.Label

import scala.collection.mutable

class FunctionSsa(private val semanticAnalysis: SemanticAnalysis, private val functionDecl: ConcreteFnDecl,
                  private val returnSink: IRSymbol,
                  private val labelMap: mutable.HashMap[Label, Block],
                  val flowGraph: Graph[Block, BlockEdge],
                  private val startBlock: Block,
                  private val endBlock: Block,
                  private val symbolTable: FunctionSymbolTable,
                  private val dominatorTree: Graph[Block, BlockEdge]
                 )