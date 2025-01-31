package util.containers.immutable

import cats.implicits.toSemigroupKOps

case class DisjointSetUnion[T] private(parent: Map[T, T], size: Map[T, Int]) {
  def find(node: T): T = parent.get(node) match
    case None => node
    case Some(p) => if p == node then p else find(p)

  def union(a: T, b: T): DisjointSetUnion[T] = {
    val pa = find(a)
    val pb = find(b)
    val paSize = size.getOrElse(pa, 1)
    val pbSize = size.getOrElse(pb, 1)

    if paSize < pbSize then
      DisjointSetUnion(parent.updated(pa, pb), size.updated(pb, paSize + pbSize))
    else
      DisjointSetUnion(parent.updated(pb, pa), size.updated(pa, paSize + pbSize))
  }

  def compressed(node: T): DisjointSetUnion[T] = {
    val newDsu = DisjointSetUnion(parent.updatedWith(node)(_ <+> Some(node)), size)
    val p = newDsu.parent(node)
    if p == node then newDsu else {
      val newDsu2 = newDsu.compressed(p)
      DisjointSetUnion(newDsu2.parent.updated(node, newDsu2.parent(p)), newDsu2.size)
    }
  }
}

object DisjointSetUnion {
  def empty[T]: DisjointSetUnion[T] = DisjointSetUnion(Map.empty, Map.empty)
}