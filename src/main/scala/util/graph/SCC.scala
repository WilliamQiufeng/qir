package util.graph

import scalax.collection.generic.{AnyEdge, Edge}
import scalax.collection.{AnyGraph, GraphLike}
import util.graph.Traversal.{transposed, traverseDfsFoldFull}

import scala.collection.mutable

object SCC {
  type ComponentMap[V] = Map[V, Int]

  def emptyComponentMap[V]: ComponentMap[V] = Map.empty

  extension [N, E <: AnyEdge[N], CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]](g: GraphLike[N, E, CC]) {
    def findSCCs(): ComponentMap[N] =
      val resComponentMap = mutable.Map.empty[N, Int]
      val finishTime: List[N] = g.traverseDfsFoldFull(g.nodes.head, List.empty[N])((a, b) => a, (m, n) => n :: m)
      val transposed = g.transposed
      var col = 0
      val stack: mutable.Stack[N] = mutable.Stack.empty[N]
      val visited: mutable.Set[N] = mutable.Set.empty[N]
      for n <- finishTime
          if !visited.contains(n)
      do
        stack.push(n)
        while stack.nonEmpty do
          val c = stack.pop
          if !visited.contains(c) then
            visited += c
            resComponentMap += c -> col
            stack.pushAll((transposed get c).diSuccessors.map(_.outer))
        col += 1
      resComponentMap.toMap
  }
}
