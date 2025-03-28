package ssa

import scalax.collection.edges.DiEdge
import scalax.collection.immutable.Graph
import scalax.collection.edges.DiEdgeImplicits
import semantic.Temp
import tac.Label

object SsaGraph {
  type SsaGraphNode = SsaBlockTac
  type EdgeType = DiEdge[SsaBlockTac]
  type GraphType = Graph[SsaBlockTac, EdgeType]

  def buildGraph(labelMap: Map[Label, SsaBlock], defUse: Map[Temp, DefUse]): GraphType = {
    val nodes =
      for block <- labelMap.values
          tac <- block.tacs
      yield SsaBlockTac(tac, block.label)

    // For each node, gather the sources' definition
    // Make an edge from each definition to the node
    val edges = nodes.flatMap(node =>
      node.tac.sources.flatMap(src =>
        defUse
          .get(src)
          .flatMap(_.definition)
          .map(_ ~> node))
    )
    Graph.from(nodes, edges)
  }
}
