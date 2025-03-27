package tac

import cats.syntax.all.*
import semantic.Temp
import util.ToStringMapped

case class TacExpression[+Impl <: TacImpl](sources: IndexedSeq[Temp], impl: Impl)

trait SomeTac {
  def sources: IndexedSeq[Temp]
  def definition: Option[Temp]
}

trait Terminator extends SomeTac {
  def targets: IndexedSeq[Label]
  final override def definition: Option[Temp] = None
}

case class Tac[+Impl <: TacImpl](sources: IndexedSeq[Temp], definition: Option[Temp], impl: Impl) extends ToStringMapped[Temp] {
  lazy val expr: TacExpression[Impl] = TacExpression(sources, impl)

  def toStringMapped[T](mapping: Temp => T): String = s"${definition.map(mapping).mkString} <- $impl ${sources.map(mapping).mkString(", ")}"
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
  override def toString = s"move"
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

case class ParallelCopy(copies: IndexedSeq[(Temp, Temp)]) extends ToStringMapped[Temp] {
  override def toStringMapped[B](mapping: Temp => B): String =
    copies.map { case (src, dst) => s"${mapping(src)} <- ${mapping(dst)}" }.mkString(" || ")

  def sources: IndexedSeq[Temp] = copies.map(_._1)
  def definitions: IndexedSeq[Temp] = copies.map(_._2)
  def add(src: Temp, dst: Temp): ParallelCopy = ParallelCopy(copies.appended((src, dst)))
}