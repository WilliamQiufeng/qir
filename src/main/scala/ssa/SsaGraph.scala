package ssa

import scalax.collection.edges.DiEdge
import scalax.collection.immutable.Graph

object SsaGraph {
  type GraphType = Graph[SsaBlockTac, DiEdge[SsaBlockTac]]

}
