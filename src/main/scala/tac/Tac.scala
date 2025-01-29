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

case class BinaryArith(op: BinaryArithOp, target: IRSymbol, arg1: IRSymbol, arg2: IRSymbol) extends Tac(Array(arg1, arg2), target.some) {
  override def toString = s"${definition.mkString} <- $op ${sources(0)} ${sources(1)}"
}

case class Move(target: IRSymbol, source: IRSymbol) extends Tac(Array(source), target.some) {
  override def toString = s"${definition.mkString} <- ${sources(0)}"
}

case class Call(target: IRSymbol, function: IRSymbol, arguments: List[IRSymbol]) extends Tac(arguments.toArray, target.some) {
  override def toString = s"${definition.mkString} <- call $function${sources.mkString("(", ", ", ")")}"
}


case class Goto(var label: Label) extends Tac(Array(), None), Jump(Array(label)) {
  override def toString: String = s"goto $label"
}

case class Branch(test: IRSymbol, var label1: Label, var label2: Label) extends Tac(Array(test), None), Jump(Array(label1, label2)) {
  override def toString: String = s"branch ${sources(0)} $label1 $label2"
}

case class Ret(value: IRSymbol) extends Tac(Array(value), None), Jump(Array()) {
  override def toString: String = s"ret ${sources(0)}"
}

case class Phi(target: IRSymbol, args: Array[IRSymbol], blocks: Array[Block]) extends Tac(args, target.some) {
  private val blockSymbolMap = Map.from(blocks zip args.indices)

  def replace(block: Block, transform: IRSymbol => IRSymbol): Boolean = {
    blockSymbolMap.get(block) match
      case Some(i) =>
        sources.update(i, transform(sources(i)))
        true
      case None => false
  }

  override def toString: String = s"${definition.mkString} <- phi ${sources.mkString("(", ", ", ")")}"
}