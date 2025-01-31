package util.graph


import scala.annotation.{tailrec, targetName}

object FixedPoint {

  import util.graph.FixedPoint.MapFixedPointState.MapType

  trait WithFixed[+T <: WithFixed[T]] {
    val fixed: Boolean

    def resetFixed: T
  }

  trait FixedPointState[T, +CC <: FixedPointState[T, CC]] extends WithFixed[CC] {
    val value: T

    @targetName("assign")
    def <<(newValue: T): CC
  }

  case class ValueFixedPointState[T](value: T, fixed: Boolean) extends FixedPointState[T, ValueFixedPointState[T]] {
    @targetName("assign")
    override def <<(newValue: T): ValueFixedPointState[T] = ValueFixedPointState(newValue, fixed && value == newValue)

    override def resetFixed: ValueFixedPointState[T] = ValueFixedPointState(value, true)
  }

  case class MapFixedPointState[T, V](value: MapType[T, V], fixed: Boolean) extends FixedPointState[MapFixedPointState.MapType[T, V], MapFixedPointState[T, V]] {
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

    override def resetFixed: MapFixedPointState[T, V] = MapFixedPointState(value, true)
  }

  object MapFixedPointState {
    type MapType[T, V] = Map[T, V]
    type SetMapType[T] = Map[T, Set[T]]
    type BijectionMapType[T] = Map[T, T]

    def empty[T, V]: MapFixedPointState[T, V] = MapFixedPointState(Map.empty[T, V], true)
  }

  type SetMapFixedPointState[T] = MapFixedPointState[T, Set[T]]
  type BijectionFixedPointState[T] = MapFixedPointState[T, T]

  extension [T <: WithFixed[T]](value: T) {
    @tailrec
    def iterateTillFixed(f: T => T): T = {
      val result = f(value.resetFixed)
      if result.fixed then result else result.iterateTillFixed(f)
    }
  }
  extension [V](map: Map[V, Set[V]]) {
    def strict: Map[V, Set[V]] = map.map((v, set) => (v, set - v))
  }
}
