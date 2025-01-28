import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.*
import flatspec.*
import matchers.*
import util.containers.mutable.DisjointSetUnion as MutableDisjointSetUnion
import util.containers.immutable.DisjointSetUnion as ImmutableDisjointSetUnion

class DSUTests extends AnyFlatSpec with should.Matchers {
  "An empty mutable DSU" should "have find(x) == x for all x" in {
    val dsu = MutableDisjointSetUnion[Int]()
    for i <- 1 to 5 do
      dsu.find(i) should be (i)
  }

  "A mutable DSU" should "union correctly" in {
    val dsu = MutableDisjointSetUnion[Int]()
    dsu.union(1, 2)
    dsu.union(3, 4)

    dsu.find(1) should be (dsu.find(2))
    dsu.find(3) should be (dsu.find(4))

    dsu.find(1) should not equal dsu.find(3)
    dsu.find(2) should not equal dsu.find(4)
    dsu.find(1) should not equal dsu.find(4)


    dsu.union(1, 3)
    dsu.find(1) should be (dsu.find(2))
    dsu.find(3) should be (dsu.find(4))
    dsu.find(2) should be (dsu.find(3))
    dsu.find(2) should be (dsu.find(4))
  }
  "An empty immutable DSU" should "have find(x) == x for all x" in {
    val dsu = ImmutableDisjointSetUnion.empty[Int]
    for i <- 1 to 5 do
      dsu.find(i) should be (i)
  }

  "An immutable DSU" should "union correctly" in {
    val dsu = ImmutableDisjointSetUnion.empty[Int].union(1, 2).union(3, 4)

    dsu.find(1) should be (dsu.find(2))
    dsu.find(3) should be (dsu.find(4))

    dsu.find(1) should not equal dsu.find(3)
    dsu.find(2) should not equal dsu.find(4)
    dsu.find(1) should not equal dsu.find(4)


    val newDsu = dsu.union(1, 3)
    newDsu.find(1) should be (newDsu.find(2))
    newDsu.find(3) should be (newDsu.find(4))
    newDsu.find(2) should be (newDsu.find(3))
    newDsu.find(2) should be (newDsu.find(4))
  }
}
