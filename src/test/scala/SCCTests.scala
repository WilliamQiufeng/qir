import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.immutable.Graph
import util.containers.immutable.DisjointSetUnion as ImmutableDisjointSetUnion
import util.containers.mutable.DisjointSetUnion as MutableDisjointSetUnion
import util.graph.SCC.findSCCs

class SCCTests extends AnyFlatSpec with should.Matchers {
  "An SCC" should "work" in {
    val graph = Graph.from(1 to 10, Iterable(
      1 ~> 2, 2 ~> 3, 3 ~> 4, 4 ~> 3, 5 ~> 1, 5 ~> 6, 2 ~> 5, 2 ~> 6,
      6 ~> 7, 7 ~> 6, 3 ~> 7, 7 ~> 8, 4 ~> 8, 8 ~> 8
    ))
    val scc = graph.findSCCs()
    println(scc)
  }
}
