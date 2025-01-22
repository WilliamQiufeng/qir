package tac

import semantic.IRSymbol
import cats.syntax.all.*

sealed trait Tac(var sources: Array[Option[IRSymbol]], var definition: Option[IRSymbol])

sealed trait Jump(var targets: Array[Label])

enum BinaryArithOp {
  case AddI
  case SubI
  case MulI
  case DivI
}

class Binary(definition_ : Option[IRSymbol], arg1: Option[IRSymbol], arg2: Option[IRSymbol]) extends Tac(Array(arg1, arg2), definition_)

class BinaryArith(op: BinaryArithOp, target: Option[IRSymbol], arg1: Option[IRSymbol], arg2: Option[IRSymbol]) extends Binary(target, arg1, arg2) {
  override def toString = s"$definition <- $op ${sources(0)} ${sources(1)}"
}

class Move(target: Option[IRSymbol], source: Option[IRSymbol]) extends Tac(Array(source), target) {
  override def toString = s"$definition <- ${sources(0)}"
}

class Call(target: Option[IRSymbol], val function: Option[IRSymbol], arguments: Array[Option[IRSymbol]]) extends Tac(arguments, target) {
  override def toString = s"$definition <- call $function${sources.mkString("(", ", ", ")")}"
}


class Goto(var label: Label) extends Tac(Array(), None), Jump(Array(label)) {
  override def toString: String = s"goto $label"
}

class Branch(test: Option[IRSymbol], var label1: Label, var label2: Label) extends Tac(Array(test), None), Jump(Array(label1, label2)) {
  override def toString: String = s"branch ${sources(0)} $label1 $label2"
}

class Ret(value: Option[IRSymbol]) extends Tac(Array(value), None), Jump(Array()) {
  override def toString: String = s"ret $value"
}

class Phi(target: Option[IRSymbol], args: Array[Option[IRSymbol]]) extends Tac(args, target) {
  override def toString: String = s"$definition <- phi ${sources.map(_.map(_.temp)).mkString("(", ", ", ")")}"
}