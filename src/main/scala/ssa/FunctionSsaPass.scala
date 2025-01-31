package ssa

import ast.{ConcreteFnDecl, LabelValue}
import dag.{FunctionInfo, WithFunctionInfo}
import scalax.collection.immutable.Graph
import semantic.{FunctionSymbolTable, IRSymbol, SemanticAnalysis, Temp}
import tac.{Block, Label, LabelEdge}

import scala.collection.mutable

abstract case class FunctionSsaPass(functionInfo: FunctionInfo, dominatorTree: Graph[Label, LabelEdge]) extends WithFunctionInfo

