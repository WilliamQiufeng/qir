package ssa

import scalax.collection.edges.DiEdge
import scalax.collection.immutable.Graph
import scalax.collection.edges.DiEdgeImplicits

object SsaGraph {
  type SsaGraphNode = SsaBlockTac
  type GraphType = Graph[SsaBlockTac, DiEdge[SsaBlockTac]]

  def buildGraph(ssaFunctionInfo: SsaFunctionInfo): GraphType = {
    val nodes =
      for block <- ssaFunctionInfo.labelMap.values
          tac <- block.tacs
      yield SsaBlockTac(tac, block.label)

    // For each node, gather the sources' definition
    // Make an edge from the node to each definition
    val edges = nodes.flatMap(node =>
      node.tac.sources.flatMap(src =>
        ssaFunctionInfo.defUse
          .get(src)
          .flatMap(_.definition)
          .map(node ~> _))
    )
    Graph.from(nodes, edges)
  }
}
