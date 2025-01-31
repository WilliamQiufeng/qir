package util.graph

import scalax.collection.edges.DiEdge
import scalax.collection.generic.Edge
import scalax.collection.immutable.Graph
import scalax.collection.{AnyGraph, GraphLike}
import util.graph.Dominance.DominanceFrontierClosureMaps.DominanceFrontierClosureMap
import util.graph.Dominance.DominanceFrontierMaps.DominanceFrontierMap
import util.graph.Dominance.DominationMaps.DominationMap
import util.graph.Dominance.ImmediateDominationMaps.ImmediateDominationMap
import util.graph.FixedPoint.{MapFixedPointState, SetMapFixedPointState}
import util.graph.Traversal.*

object Dominance {
  object DominationMaps {
    opaque type DominationMap[T] = Map[T, Set[T]]
    extension [T](m: DominationMap[T]) {
      def dom(a: T, b: T): Boolean = m.get(b).exists(_.contains(a))
      def sdom(a: T, b: T): Boolean = dom(a, b) && a != b
      def sdoms(a: T): Set[T] = m.get(a).map(_.excl(a)).getOrElse(Set.empty)
      def apply(key: T): Set[T] = m(key)
    }

    object DominationMap {
      def apply[T](map: Map[T, Set[T]]): DominationMap[T] = map
    }
  }

  object ImmediateDominationMaps {
    opaque type ImmediateDominationMap[T] = Map[T, T]
    extension [T](m: ImmediateDominationMap[T]) {
      def idom(a: T, b: T): Boolean = m.get(b).contains(a)
      def apply(key: T): Option[T] = m.get(key)
    }

    object ImmediateDominationMap {
      def apply[T](map: Map[T, T]): ImmediateDominationMap[T] = map
    }
  }

  object DominanceFrontierMaps {
    opaque type DominanceFrontierMap[T] = Map[T, Set[T]]
    extension [T](m: DominanceFrontierMap[T]) {
      def apply(key: T): Set[T] = m.getOrElse(key, Set.empty)
    }

    object DominanceFrontierMap {
      def apply[T](map: Map[T, Set[T]]): DominanceFrontierMap[T] = map
    }
  }

  object DominanceFrontierClosureMaps {
    opaque type DominanceFrontierClosureMap[T] = Map[T, Set[T]]
    extension [T](m: DominanceFrontierClosureMap[T]) {
      def apply(key: T): Set[T] = m.getOrElse(key, Set.empty)
    }

    object DominanceFrontierClosureMap {
      def apply[T](map: Map[T, Set[T]]): DominanceFrontierClosureMap[T] = map
    }
  }

  case class DominanceInfo[T](dominationMap: DominationMap[T],
                              immediateDominationMap: ImmediateDominationMap[T],
                              dominanceFrontierMap: DominanceFrontierMap[T],
                              dominanceFrontierClosureMap: DominanceFrontierClosureMap[T],
                              dominationTree: Graph[T, DiEdge[T]])

  extension [N, E <: Edge[N], CC[X, Y <: Edge[X]] <: GraphLike[X, Y, CC] with AnyGraph[X, Y]](g: GraphLike[N, E, CC]) {
    private def calculateDominatorsFold(dominatorCalculationState: SetMapFixedPointState[N], curBlock: g.NodeT): SetMapFixedPointState[N] = {
      val predDoms = curBlock.diPredecessors.map(dominatorCalculationState.value(_))
      val intersection = if predDoms.isEmpty then Set.empty else predDoms.foldLeft(predDoms.head)(_.intersect(_))
      val newDoms = intersection.incl(curBlock)
      dominatorCalculationState << (curBlock.outer -> newDoms)
    }

    def calculateDominators(startBlock: N): DominationMap[N] = {
      val nodeSet = g.nodes.outerIterable.toSet
      val initialMap: Map[N, Set[N]] = Map.from(nodeSet.map(b => b -> (if b == startBlock then Set(b) else nodeSet)))

      val res: SetMapFixedPointState[N] = MapFixedPointState(initialMap, false)
        .iterateTillFixed(x => g.traverseBfsFold(g get startBlock)(x)(calculateDominatorsFold))
      DominationMap(res.value)
    }
    def calculateIDom(startBlock: N): ImmediateDominationMap[N] =
      calculateIDomFromSdoms(startBlock, g.calculateDominators(startBlock))

    def calculateIDomFromSdoms(startBlock: N, doms: DominationMap[N]): ImmediateDominationMap[N] =
      ImmediateDominationMap(
        g.nodes.outerIterable.map(b => {
          val sdom = doms.sdoms(b)
          b -> sdom.find(d => sdom.excl(d).forall(!doms.sdoms(_).contains(d)))
        }).collect({ case (k, Some(v)) => (k, v) }).toMap
      )

    def calculateDominanceFrontiers(doms: DominationMap[N], idoms: ImmediateDominationMap[N]): DominanceFrontierMap[N] = {
      var dominanceFrontiers = Map.empty[N, Set[N]]
      for edge <- g.edges do
        var x = edge.sources.head.outer
        while !doms.sdom(x, edge.targets.head) do
          dominanceFrontiers = dominanceFrontiers.updatedWith(x)(s => Some(s.getOrElse(Set.empty).incl(edge.targets.head)))
          x = idoms(x).get

      DominanceFrontierMap(dominanceFrontiers)
    }

    def calculateDominanceFrontierClosures(dominanceFrontierMap: DominanceFrontierMap[N]): DominanceFrontierClosureMap[N] = {
      val initial = g.nodes.outerIterable.map(b => b -> dominanceFrontierMap(b)).toMap
      val res = MapFixedPointState(initial, false).iterateTillFixed(currentState =>
        currentState << currentState.value.map((block, dfClosure) =>
          block -> dfClosure.foldLeft(dfClosure)((accDfClosure, df) => accDfClosure | currentState.value(df))))
      DominanceFrontierClosureMap(res.value)
    }
    def makeDomTree(immediateDominationMap: ImmediateDominationMap[N]): Graph[N, DiEdge[N]] =
      Graph.from(g.nodes.outerIterable, g.nodes.flatMap(b => immediateDominationMap(b).map(x => DiEdge(x, b))))

    def makeDominanceInfo(startNode: N): DominanceInfo[N] = {
      val domMap = calculateDominators(startNode)
      val idomMap = calculateIDomFromSdoms(startNode, domMap)
      val df = calculateDominanceFrontiers(domMap, idomMap)
      val dfClosure = calculateDominanceFrontierClosures(df)
      val domTree = makeDomTree(idomMap)
      DominanceInfo(domMap, idomMap, df, dfClosure, domTree)
    }
  }
}
