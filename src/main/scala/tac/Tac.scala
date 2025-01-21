package tac

import semantic.IRSymbol
import cats.syntax.all.*

trait Tac(var sources: Array[IRSymbol], var definition: Option[IRSymbol])

sealed trait Jump(var targets: Array[Label])

enum BinaryArithOp {
  case AddI
  case SubI
  case MulI
  case DivI
}

class Binary(definition: IRSymbol, arg1: IRSymbol, arg2: IRSymbol) extends Tac(Array(arg1, arg2), definition.some)

class BinaryArith(op: BinaryArithOp, definition: IRSymbol, arg1: IRSymbol, arg2: IRSymbol) extends Binary(definition, arg1, arg2) {
  override def toString = s"$definition <- $op $arg1 $arg2"
}

class Move(definition: IRSymbol, source: IRSymbol) extends Tac(Array(source), definition.some) {
  override def toString = s"$definition <- $source"
}

class Call(definition: IRSymbol, val function: IRSymbol, arguments: List[IRSymbol]) extends Tac(arguments.toArray, definition.some) {
  override def toString = s"$definition <- call $function${arguments.mkString("(", ", ", ")")}"
}


class Goto(var label: Label) extends Tac(Array(), None), Jump(Array(label)) {
  override def toString: String = s"goto $label"
}

class Branch(test: IRSymbol, var label1: Label, var label2: Label) extends Tac(Array(test), None), Jump(Array(label1, label2)) {
  override def toString: String = s"branch $test $label1 $label2"
}

class Ret(value: IRSymbol) extends Tac(Array(value), None), Jump(Array()) {
  override def toString: String = s"ret $value"
}
