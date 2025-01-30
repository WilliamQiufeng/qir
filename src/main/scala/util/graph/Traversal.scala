package util.graph

import scalax.collection.{AnyGraph, GraphLike}
import scalax.collection.generic.Edge

import scala.collection.mutable

object Traversal {
  extension [N, E <: Edge[N], CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]](graph: GraphLike[N, E, CC]) {
    def traverseDfs(start: graph.NodeT)(f: graph.NodeT => Unit): Unit = {
      val dfsStack = mutable.Stack(graph get start)
      val visited: mutable.Set[graph.NodeT] = mutable.Set.empty
      while dfsStack.nonEmpty do
        val b = dfsStack.pop()
        if visited.add(b) then
          dfsStack.pushAll((graph get b).diSuccessors)
          f(b)
    }
    def traverseBfs(start: graph.NodeT)(f: graph.NodeT => Unit): Unit = {
      val bfsQueue = mutable.Queue(graph get start)
      val visited: mutable.Set[graph.NodeT] = mutable.Set.empty
      while bfsQueue.nonEmpty do
        val b = bfsQueue.dequeue()
        if visited.add(b) then
          bfsQueue.enqueueAll((graph get b).diSuccessors)
          f(b)
    }
    def traverseBfsFold[T](startNode: graph.NodeT)(initialValue: T)(f: (T, graph.NodeT) => T): T = {
      val bfsQueue = mutable.Queue(graph get startNode)
      val visited: mutable.Set[graph.NodeT] = mutable.Set.empty
      var value = initialValue
      while bfsQueue.nonEmpty do
        val b = bfsQueue.dequeue()
        if visited.add(b) then
          bfsQueue.enqueueAll((graph get b).diSuccessors)
          value = f(value, b)
      value
    }
  }
}
