package tac

import cats.syntax.all.*
import semantic.Temp
import util.ToStringMapped

import scala.collection.View

case class TacExpression[+Impl <: Tac](sources: IndexedSeq[Temp], impl: Impl)

trait Tac extends ToStringMapped[Temp] {
  def sources: IndexedSeq[Temp]

  def definitions: IndexedSeq[Temp]

  def operationName: String

  def rewrite(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]): Tac

  override def toStringMapped[B](mapping: Temp => B): String = s"${definitions.map(mapping).mkString(", ")} <- $operationName ${sources.map(mapping).mkString(", ")}"
}

trait NormalTac extends Tac

trait SingleDefinition {
  this: Tac =>
  def definition: Temp

  final override def definitions: IndexedSeq[Temp] = IndexedSeq(definition)
}

trait NoDefinition {
  this: Tac =>
  final override def definitions: IndexedSeq[Temp] = IndexedSeq.empty

  override def toStringMapped[B](mapping: Temp => B): String = s"$operationName ${sources.map(mapping).mkString(", ")}"
}

trait SingleSource {
  this: Tac =>
  def source: Temp

  final override def sources: IndexedSeq[Temp] = IndexedSeq(source)
}

trait NoSource {
  this: Tac =>
  final override def sources: IndexedSeq[Temp] = IndexedSeq.empty

  override def toStringMapped[B](mapping: Temp => B): String = s"${definitions.map(mapping).mkString(", ")} <- $operationName"
}

trait NoDefinitionAndSource extends NoDefinition, NoSource {
  this: Tac =>
  override def toStringMapped[B](mapping: Temp => B): String = operationName
}

trait Terminator extends Tac, NoDefinition {
  def targets: IndexedSeq[Label]
}

enum BinaryArithOp {
  case AddI
  case SubI
  case MulI
  case DivI
}

case class BinaryArith(definition: Temp, left: Temp, right: Temp, op: BinaryArithOp) extends NormalTac, SingleDefinition {
  override def sources: IndexedSeq[Temp] = IndexedSeq(left, right)

  override def operationName: String = op.toString

  override def rewrite(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]): BinaryArith = BinaryArith(definitions.head, sources(0), sources(1), op)
}

case class Move(definition: Temp, source: Temp) extends NormalTac, SingleDefinition, SingleSource {
  override def toString = s"$definition <- $source"

  override def operationName: String = "move"

  override def rewrite(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]): Move = Move(definitions.head, sources.head)
}

case class Call(definition: Temp, sources: IndexedSeq[Temp], fnName: String) extends NormalTac, SingleDefinition {
  override def operationName: String = s"call $fnName"

  override def rewrite(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]): Call = {
    val res = Call(definitions.head, sources, fnName)
    println(s"$this -> $res")
    res
  }
}

case class Goto(label: Label) extends Terminator, NoDefinitionAndSource {
  override val targets: IndexedSeq[Label] = IndexedSeq(label)

  override def operationName: String = s"goto $label"

  override def rewrite(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]): Goto = Goto(label)
}

case class Branch(source: Temp, label1: Label, label2: Label) extends Terminator, NoDefinition, SingleSource {
  override def operationName: String = s"branch $label1/$label2"

  override val targets: IndexedSeq[Label] = IndexedSeq(label1, label2)

  override def rewrite(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]): Branch = Branch(sources.head, label1, label2)
}

case class Ret(source: Temp) extends Terminator, NoDefinition, SingleSource {
  override def targets: IndexedSeq[Label] = IndexedSeq.empty

  override def operationName: String = "ret"

  override def rewrite(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]): Ret = Ret(sources.head)
}

case class Phi(definition: Temp, sources: IndexedSeq[Temp], blockLabels: IndexedSeq[Label]) extends Tac, SingleDefinition {
  private val labelIndex = Map.from(blockLabels.zipWithIndex)

  def replace(label: Label, transform: Temp => Temp, tac: Phi): Phi = {
    labelIndex.get(label) match
      case Some(i) =>
        tac.copy(sources = tac.sources.updated(i, transform(tac.sources(i))))
      case None => tac
  }

  override def toStringMapped[B](mapping: Temp => B): String = 
    s"${mapping(definition)} <- phi ${sources.zip(blockLabels).map { case (src, label) => s"${mapping(src)} : $label" }.mkString(", ")}"

  override def operationName: String = "phi"

  override def rewrite(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]): Phi = Phi(definitions.head, sources, blockLabels)
}

case class ParallelCopy(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]) extends Tac, ToStringMapped[Temp] {
  override def toStringMapped[B](mapping: Temp => B): String =
    copies.map { case (src, dst) => s"${mapping(src)} <- ${mapping(dst)}" }.mkString(" || ")

  def copies: View[(Temp, Temp)] = definitions.view.zip(sources)

  def add(src: Temp, dst: Temp): ParallelCopy = ParallelCopy(definitions.appended(dst), sources.appended(src))

  override def operationName: String = "parallel_copy"

  override def rewrite(definitions: IndexedSeq[Temp], sources: IndexedSeq[Temp]): Tac =
    ParallelCopy(definitions, sources)
}