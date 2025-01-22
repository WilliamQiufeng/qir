package tac

import semantic.IRSymbol
import cats.syntax.all.*
import dag.Block

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
  override def toString = s"$definition <- $op ${sources(0)} ${sources(1)}"
}

class Move(target: IRSymbol, source: IRSymbol) extends Tac(Array(source), target.some) {
  override def toString = s"$definition <- ${sources(0)}"
}

class Call(target: IRSymbol, val function: IRSymbol, arguments: List[IRSymbol]) extends Tac(arguments.toArray, target.some) {
  override def toString = s"$definition <- call $function${sources.mkString("(", ", ", ")")}"
}


class Goto(var label: Label) extends Tac(Array(), None), Jump(Array(label)) {
  override def toString: String = s"goto $label"
}

class Branch(test: IRSymbol, var label1: Label, var label2: Label) extends Tac(Array(test), None), Jump(Array(label1, label2)) {
  override def toString: String = s"branch ${sources(0)} $label1 $label2"
}

class Ret(value: IRSymbol) extends Tac(Array(value), None), Jump(Array()) {
  override def toString: String = s"ret $value"
}

class Phi(target: IRSymbol, args: Array[IRSymbol], blocks: Array[Block]) extends Tac(args, target.some) {
  private val blockSymbolMap = Map.from(blocks zip args.indices)

  def replace(block: Block, transform: IRSymbol => IRSymbol): Boolean = {
    blockSymbolMap.get(block) match
      case Some(i) =>
        sources.update(i, transform(sources(i)))
        true
      case None => false
  }
  override def toString: String = s"$definition <- phi ${sources.map(_.temp).mkString("(", ", ", ")")}"
}