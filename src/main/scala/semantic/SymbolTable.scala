package semantic

import cats.syntax.all.*
import tac.BlockTac

import scala.collection.mutable

sealed trait IRSymbol(val temp: Temp, val ty: Type,
                      val debugName: Option[String] = None,
                      val undefined: Boolean = false)

case class NormalIRSymbol(temp: Temp, ty: Type, debugName: Option[String] = None, undefined: Boolean = false)
  extends IRSymbol(temp, ty, debugName, undefined) {
  def toString = s"${debugName.mkString}($temp)${if undefined then "⊥" else ""}"
}

sealed trait SSASymbol extends IRSymbol

case class SSARootSymbol(temp: Temp, ty: Type,
                         debugName: Option[String] = None,
                         undefined: Boolean = false)
  extends SSASymbol with IRSymbol(temp, ty, debugName, undefined) {
  def toString = s"${debugName.mkString}($temp)${if undefined then "⊥" else ""}"

  var uses: Set[BlockTac] = Set.empty
  val stack: mutable.Stack[IRSymbol] = mutable.Stack.empty
}

case class SSADerivedSymbol(origin: SSARootSymbol) extends SSASymbol with IRSymbol(Temp(), origin.ty) {
  override def toString = s"$origin.$temp"
}

case class ConstIRSymbol(const: ast.Const,
                         temp: Temp,
                         debugName: Option[String] = None) extends SSASymbol, IRSymbol(temp, const match
  case ast.ConstInteger(_) => IntType
  case ast.ConstFloat(_) => FloatType
  case ast.ConstString(_) => PointerType(() => CharType)
  case ast.ConstChar(_) => CharType
  case ast.ConstUnit => UnitType, debugName) {

  override def toString = s"$const($temp)"
}

sealed trait SymbolTable[KeyType, SymbolType <: IRSymbol] {
  def lookup(name: KeyType): Option[SymbolType]

  def remove(name: KeyType): this.type

  def insert(name: KeyType, sym: SymbolType): this.type

  def modify(name: KeyType, f: Option[SymbolType] => Option[SymbolType]): this.type = {
    f(lookup(name)) match
      case None => remove(name)
      case Some(value) => insert(name, value)
  }

  def values: Iterable[SymbolType]
}

private trait HashMapSymbolTable[SymbolType <: IRSymbol](val map: Map[String, SymbolType] = Map.empty) extends SymbolTable[String, SymbolType] {

  override def lookup(name: String): Option[SymbolType] = map.get(name)

  def values: Iterable[IRSymbol] = map.values

  override def toString = s"${getClass.getSimpleName}(${map.mkString("{\n  ", "\n  ", "\n}")})"
}

case class StructSymbolTable(fieldTypes: List[NormalIRSymbol] = List.empty) extends SymbolTable[Int, NormalIRSymbol] {
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

case class FunctionSymbolTable(map: Map[String, NormalIRSymbol] = Map.empty) extends HashMapSymbolTable(map) {
  def insert(name: String, symbol: NormalIRSymbol): FunctionSymbolTable = {
    FunctionSymbolTable(map.updated(name, symbol))
  }

  override def remove(name: String): FunctionSymbolTable = FunctionSymbolTable(map - name)
}

case class GlobalSymbolTable(map: Map[String, IRSymbol] = Map.empty) extends HashMapSymbolTable(map) {
  def insert(name: String, symbol: IRSymbol): GlobalSymbolTable = {
    GlobalSymbolTable(map.updated(name, symbol))
  }
}