package ssa

import semantic.{ConstIRSymbol, SsaSymbol, Temp}
import tac.*
import util.ToStringMapped

import scala.collection.View

case class SsaBlockTac(tac: Tac, label: Label) extends ToStringMapped[Temp] {
  override def toStringMapped[T](mapping: Temp => T): String = s"(${tac.toStringMapped(mapping)} at $label)"
}

trait SsaBlock extends Block, ToStringMapped[Temp] {
  def phis: List[Phi]

  def trailingTacs: View[Tac]

  def tacs: View[Tac] = phis.view.appendedAll(trailingTacs)

  def getNextBlockIfEmpty(tempMap: Map[Temp, SsaSymbol]): Option[Label] = {
    if phis.nonEmpty || trailingTacs.size != 1 then
      return None
    terminator match
      case Goto(label) => Some(label)
      case Branch(test, label1, label2) => tempMap(test) match
        case ConstIRSymbol(ast.ConstInteger(v), _, _, _) => Some(if v > 0 then label1 else label2)
        case _ => None
      case Ret(_) => None
  }

  def rewrite(label: Label, phis: List[Phi], trailingTacs: IndexedSeq[Tac]): SsaBlock
}

case class BasicSsaBlock(label: Label, phis: List[Phi], normalTacs: IndexedSeq[NormalTac], terminator: Terminator) extends SsaBlock {
  override def trailingTacs: View[Tac] = normalTacs.view.appended(terminator)

  override def toStringMapped[T](mapping: Temp => T): String =
    s"Block $label {\n  ${tacs.map(_.toStringMapped(mapping)).mkString("\n  ")}}"

  def self: BasicSsaBlock = this

  override def rewrite(label: Label, phis: List[Phi], trailingTacs: IndexedSeq[Tac]): BasicSsaBlock =
    BasicSsaBlock(label, phis, trailingTacs.slice(0, trailingTacs.size - 1).map(_.asInstanceOf[NormalTac]), trailingTacs.last.asInstanceOf)
}

case class SsaBlockPc(label: Label,
                      phis: List[Phi],
                      pcAfterPhi: ParallelCopy,
                      normalTacs: IndexedSeq[NormalTac],
                      pcAtEnd: ParallelCopy,
                      terminator: Terminator)
  extends SsaBlock {

  override def trailingTacs: View[Tac] = Seq(pcAfterPhi).view.appendedAll(normalTacs).appended(pcAtEnd).appended(terminator)

  override def toStringMapped[B](mapping: Temp => B): String =
    s"Block $label {\n  ${tacs.map(_.toStringMapped(mapping)).mkString("\n  ")}}"

  override def rewrite(label: Label, phis: List[Phi], trailingTacs: IndexedSeq[Tac]): SsaBlockPc =
    SsaBlockPc(label,
      phis,
      trailingTacs.head.asInstanceOf,
      trailingTacs.slice(1, trailingTacs.size - 2).map(_.asInstanceOf),
      trailingTacs(trailingTacs.size - 2).asInstanceOf,
      trailingTacs.last.asInstanceOf)
}