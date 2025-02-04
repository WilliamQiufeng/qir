package ssa

import dag.{FunctionInfo, WithFunctionInfo}
import scalax.collection.immutable.Graph
import tac.{Label, LabelEdge}


abstract case class FunctionSsaPass(functionInfo: FunctionInfo, dominatorTree: Graph[Label, LabelEdge]) extends WithFunctionInfo

