package tac

import cats.syntax.all.*
import semantic.Temp

case class Tac[Impl <: TacImpl](sources: IndexedSeq[Temp], definition: Option[Temp], impl: Impl) {
  override def toString: String = toStringNamed(x => x)

  def toStringNamed[T](mapping: Temp => T): String = s"${definition.map(mapping).mkString} <- $impl ${sources.map(mapping).mkString(", ")}"
}

sealed trait TacImpl

sealed trait Jump(var targets: IndexedSeq[Label]) extends TacImpl

enum BinaryArithOp {
  case AddI
  case SubI
  case MulI
  case DivI
}

case class BinaryArith(op: BinaryArithOp) extends TacImpl {
  override def toString = s"$op"
}

case object Move extends TacImpl {
  override def toString = s""
}

case class Call(fnName: String) extends TacImpl {
  override def toString = s"call $fnName,"
}

case class Goto(label: Label) extends Jump(IndexedSeq(label)) {
  override def toString: String = s"goto $label"
}

case class Branch(label1: Label, label2: Label) extends Jump(IndexedSeq(label1, label2)) {
  override def toString: String = s"branch $label1 $label2"
}

case object Ret extends Jump(IndexedSeq()) {
  override def toString: String = "ret"
}

case class Phi(blockLabels: IndexedSeq[Label]) extends TacImpl {
  private val labelIndex = Map.from(blockLabels.zipWithIndex)

  def replace(label: Label, transform: Temp => Temp, tac: Tac[Phi]): Tac[Phi] = {
    labelIndex.get(label) match
      case Some(i) =>
        tac.copy(sources = tac.sources.updated(i, transform(tac.sources(i))))
      case None => tac
  }

  override def toString: String = "phi"
}