package semantic

import cats.syntax.all.*
import dag.BlockTac

import scala.collection.mutable

class IRSymbol(val temp: Temp, var ty: Type,
               var debugName: Option[String] = None, val undefined: Boolean = false) {
  override def toString = s"${debugName.mkString}($temp)${if undefined then "⊥" else ""}"

  var uses: Set[BlockTac] = Set.empty
  val stack: mutable.Stack[IRSymbol] = mutable.Stack.empty
}

class SSASymbol(val origin: IRSymbol) extends IRSymbol(Temp(), origin.ty) {
  debugName = s"${origin.debugName.mkString}.$temp".some
}

class ConstIRSymbol(val const: ast.Const, temp: Temp) extends IRSymbol(temp, const match
  case ast.ConstInteger(_) => IntType
  case ast.ConstFloat(_) => FloatType
  case ast.ConstString(_) => PointerType(() => CharType)
  case ast.ConstChar(_) => CharType
  case ast.ConstUnit => UnitType) {

  override def toString = s"$const($temp)"
}

sealed trait SymbolTable {
  def lookup(name: String): Option[IRSymbol]
}

private class HashMapSymbolTable extends SymbolTable {
  private val map = new mutable.HashMap[String, IRSymbol]()

  override def lookup(name: String): Option[IRSymbol] = map.get(name)

  def insert(name: String, symbol: IRSymbol): IRSymbol = {
    map.addOne((name, symbol))
    symbol
  }

  def values: Iterable[IRSymbol] = map.values

  override def toString = s"${getClass.getSimpleName}(${map.mkString("{\n  ", "\n  ", "\n}")})"
}

class StructSymbolTable(val names: List[String], val types: List[Type]) extends HashMapSymbolTable {
  names.zip(types).foreach((n, t) => insert(n, IRSymbol(Temp(), t)))
}

class FunctionSymbolTable extends HashMapSymbolTable

class GlobalSymbolTable extends HashMapSymbolTable