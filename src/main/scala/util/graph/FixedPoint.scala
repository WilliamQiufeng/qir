package util.graph


import scala.annotation.{tailrec, targetName}

object FixedPoint {

  import util.graph.FixedPoint.MapFixedPointState.MapType

  trait FixedPointState[T] {
    val value: T
    val fixed: Boolean

    @targetName("assign")
    def <<(newValue: T): FixedPointState[T]
  }

  case class ValueFixedPointState[T](value: T, fixed: Boolean) extends FixedPointState[T] {
    @targetName("assign")
    override def <<(newValue: T): FixedPointState[T] = ValueFixedPointState(newValue, fixed && value == newValue)
  }

  case class MapFixedPointState[T, V](value: MapType[T, V], fixed: Boolean) extends FixedPointState[MapFixedPointState.MapType[T, V]] {
    @targetName("assign")
    override def <<(newMap: MapType[T, V]): MapFixedPointState[T, V] = {
      MapFixedPointState(newMap, fixed && value == newMap)
    }

    @targetName("assign")
    def <<(newPair: (T, V)): MapFixedPointState[T, V] = {
      if value.get(newPair._1).contains(newPair._2) then
        this
      else
        MapFixedPointState(value + newPair, false)
    }
  }

  object MapFixedPointState {
    type MapType[T, V] = Map[T, V]
    type SetMapType[T] = Map[T, Set[T]]
    type BijectionMapType[T] = Map[T, T]

    def empty[T, V]: MapFixedPointState[T, V] = MapFixedPointState(Map.empty[T, V], true)
  }

  type SetMapFixedPointState[T] = MapFixedPointState[T, Set[T]]
  type BijectionFixedPointState[T] = MapFixedPointState[T, T]

  extension [T](value: T) {
    @tailrec
    def iterateTillFixed(f: T => (T, Boolean)): T = {
      val (result, fixed) = f(value)
      if fixed then value else value.iterateTillFixed(f)
    }
  }
  extension [V](map: Map[V, Set[V]]) {
    def strict: Map[V, Set[V]] = map.map((v, set) => (v, set - v))
  }
}
