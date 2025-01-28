package util.containers.mutable

import scala.collection.mutable

class DisjointSetUnion[T] {
  private val parent: mutable.HashMap[T, T] = mutable.HashMap.empty
  private val size: mutable.HashMap[T, Int] = mutable.HashMap.empty

  private def ensureExists(node: T): Unit = {
    if !parent.contains(node) then
      parent.addOne(node, node)
      size.addOne(node, 1)
  }

  def find(node: T): T = parent.updateWith(node) {
    case None =>
      size.addOne(node, 1)
      Some(node)
    case Some(p) => if p == node then Some(p) else Some(find(p))
  }.get

  def union(a: T, b: T): Unit = {
    ensureExists(a)
    ensureExists(b)
    val pa = find(a)
    val pb = find(b)

    if size(pa) < size(pb) then
      parent.update(pa, pb)
      size.update(pb, size(pb) + size(pa))
    else
      parent.update(pb, pa)
      size.update(pa, size(pa) + size(pb))
  }
}