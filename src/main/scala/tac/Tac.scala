package tac

import semantic.IRSymbol
import cats.syntax.all.*

sealed trait Tac(var sources: Array[IRSymbol], var definition: Option[IRSymbol])

sealed trait Jump(var targets: Array[Label])

enum BinaryArithOp {
  case AddI
  case SubI
  case MulI
  case DivI
}

class Binary(definition: IRSymbol, arg1: IRSymbol, arg2: IRSymbol) extends Tac(Array(arg1, arg2), definition.some)

class BinaryArith(op: BinaryArithOp, target: IRSymbol, arg1: IRSymbol, arg2: IRSymbol) extends Binary(target, arg1, arg2) {
  override def toString = s"$definition <- $op $sources"
}

class Move(target: IRSymbol, source: IRSymbol) extends Tac(Array(source), target.some) {
  override def toString = s"$definition <- $sources"
}

class Call(definition: IRSymbol, val function: IRSymbol, arguments: List[IRSymbol]) extends Tac(arguments.toArray, definition.some) {
  override def toString = s"$definition <- call $function${sources.mkString("(", ", ", ")")}"
}


class Goto(var label: Label) extends Tac(Array(), None), Jump(Array(label)) {
  override def toString: String = s"goto $label"
}

class Branch(test: IRSymbol, var label1: Label, var label2: Label) extends Tac(Array(test), None), Jump(Array(label1, label2)) {
  override def toString: String = s"branch $sources $label1 $label2"
}

class Ret(value: IRSymbol) extends Tac(Array(value), None), Jump(Array()) {
  override def toString: String = s"ret $value"
}

class Phi(target: IRSymbol, args: Array[IRSymbol]) extends Tac(args, target.some) {
  override def toString: String = s"$definition <- phi ${sources.map(_.temp).mkString("(", ", ", ")")}"
}