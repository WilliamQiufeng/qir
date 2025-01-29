package ssa

import ast.{ConcreteFnDecl, LabelValue}
import dag.{FunctionInfo, FunctionPass}
import scalax.collection.immutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, SemanticAnalysis, Temp}
import tac.{Block, BlockEdge, Label}

import scala.collection.mutable

abstract class FunctionSsaPass(val functionInfo: FunctionInfo,
                      val dominatorTree: Graph[Block, BlockEdge]
                 ) extends FunctionPass(functionInfo)

