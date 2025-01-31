package semantic

import cats.syntax.all.*
import tac.BlockTac

import scala.collection.mutable

sealed trait IRSymbol:
  val temp: Temp
  val ty: Type
  val debugName: Option[String]
  val undefined: Boolean

case class NormalIRSymbol(temp: Temp, ty: Type, debugName: Option[String] = None, undefined: Boolean = false)
  extends IRSymbol {
  override def toString = s"${debugName.mkString}($temp)"
}

case class TypeIRSymbol(ty: Type, debugName: Option[String] = None) extends IRSymbol {
  override val temp: Temp = Temp()
  override val undefined: Boolean = false
}

sealed trait SsaSymbol extends IRSymbol {
  def origin: SsaRootSymbol
}

sealed trait SsaRootSymbol extends SsaSymbol {
  def origin: SsaRootSymbol = this
}

case class SsaNormalSymbol(temp: Temp, ty: Type,
                           debugName: Option[String] = None, undefined: Boolean = false) extends SsaRootSymbol {
  override def toString = s"${debugName.mkString}($temp)"
  override def origin: SsaRootSymbol = this
  var uses: Set[BlockTac] = Set.empty
}

case class SsaDerivedSymbol(origin: SsaRootSymbol) extends SsaSymbol {
  override def toString = s"$origin.$temp"

  override val temp: Temp = Temp()
  override val ty: Type = origin.ty
  override val debugName: Option[String] = Some(s"${origin.debugName}.${temp.id}")
  override val undefined: Boolean = false
}

case class ConstIRSymbol(const: ast.Const,
                         temp: Temp,
                         ty: Type,
                         debugName: Option[String] = None) extends SsaRootSymbol {
  override def toString = s"$const($temp)"

  override val undefined: Boolean = false
  override def origin: SsaRootSymbol = this
}

sealed trait SymbolTable[KeyType, SymbolType <: IRSymbol, +CC <: SymbolTable[KeyType, SymbolType, CC]] {
  def lookup(name: KeyType): Option[SymbolType]

  def remove(name: KeyType): CC

  def insert(name: KeyType, sym: SymbolType): CC

  def modify(name: KeyType, f: Option[SymbolType] => Option[SymbolType]): CC = {
    f(lookup(name)) match
      case None => remove(name)
      case Some(value) => insert(name, value)
  }

  def values: Iterable[SymbolType]
}

private trait HashMapSymbolTable[SymbolType <: IRSymbol, +CC <: HashMapSymbolTable[SymbolType, CC]] extends SymbolTable[String, SymbolType, CC] {
  val map: Map[String, SymbolType] = Map.empty

  override def lookup(name: String): Option[SymbolType] = map.get(name)

  def values: Iterable[SymbolType] = map.values

  override def toString = s"${getClass.getSimpleName}(${map.mkString("{\n  ", "\n  ", "\n}")})"
}

case class StructSymbolTable(fieldTypes: List[NormalIRSymbol] = List.empty) extends SymbolTable[Int, NormalIRSymbol, StructSymbolTable] {
  override def insert(name: Int, symbol: NormalIRSymbol): StructSymbolTable = {
    val (pre, tail) = fieldTypes.splitAt(name)
    StructSymbolTable(pre ++ (symbol :: tail))
  }

  override def lookup(name: Int): Option[NormalIRSymbol] = fieldTypes.get(name)

  override def remove(name: Int): StructSymbolTable = {
    val (pre, tail) = fieldTypes.splitAt(name)
    StructSymbolTable(pre ++ tail.drop(1))
  }

  def values: Iterable[NormalIRSymbol] = fieldTypes
}

object StructSymbolTable {
  def from(types: List[Type]): StructSymbolTable =
    StructSymbolTable(types map (x => NormalIRSymbol(Temp(), x)))
}

case class FunctionSymbolTable(symbolMap: Map[String, NormalIRSymbol] = Map.empty) extends HashMapSymbolTable[NormalIRSymbol, FunctionSymbolTable] {
  override val map: Map[String, NormalIRSymbol] = symbolMap
  override def insert(name: String, symbol: NormalIRSymbol): FunctionSymbolTable = {
    FunctionSymbolTable(map.updated(name, symbol))
  }

  override def remove(name: String): FunctionSymbolTable = FunctionSymbolTable(map - name)
}

case class GlobalSymbolTable(symbolMap: Map[String, IRSymbol] = Map.empty) extends HashMapSymbolTable[IRSymbol, GlobalSymbolTable] {
  override val map: Map[String, IRSymbol] = symbolMap
  override def insert(name: String, symbol: IRSymbol): GlobalSymbolTable = {
    GlobalSymbolTable(map.updated(name, symbol))
  }

  override def remove(name: String): GlobalSymbolTable = GlobalSymbolTable(map - name)
}

case class TypeSymbolTable(symbolMap: Map[String, TypeIRSymbol] = Map.empty) extends HashMapSymbolTable[TypeIRSymbol, TypeSymbolTable] {
  override val map: Map[String, TypeIRSymbol] = symbolMap
  override def insert(name: String, symbol: TypeIRSymbol): TypeSymbolTable = {
    TypeSymbolTable(map.updated(name, symbol))
  }

  override def remove(name: String): TypeSymbolTable = TypeSymbolTable(map - name)
}