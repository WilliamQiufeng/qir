package ssa

import semantic.{ConstIRSymbol, SsaSymbol, Temp}
import tac.*
import util.ToStringMapped

import scala.collection.SeqView

case class SsaBlockTac(tac: Tac, label: Label) extends ToStringMapped[Temp] {
  override def toStringMapped[T](mapping: Temp => T): String = s"(${tac.toStringMapped(mapping)} at $label)"
}

trait SsaBlock {
  def label: Label

  def phis: List[Phi]

  def trailingTacs: IndexedSeq[Tac]

  def getNextBlockIfEmpty(tempMap: Map[Temp, SsaSymbol]): Option[Label] = {
    if phis.nonEmpty || trailingTacs.size != 1 then
      return None
    trailingTacs.last match
      case jump: Terminator => jump match
        case Goto(label) => Some(label)
        case Branch(_, label1, label2) => tempMap(trailingTacs.last.sources.head) match
          case ConstIRSymbol(ast.ConstInteger(v), _, _, _) => Some(if v > 0 then label1 else label2)
          case _ => None
        case Ret(_) => None
      case _ => throw new Exception("What")
  }
}

case class BasicSsaBlock(label: Label, phis: List[Phi], trailingTacs: IndexedSeq[Tac]) extends SsaBlock, ToStringMapped[Temp] {

  def tacs: SeqView[Tac] = trailingTacs.view.prependedAll(phis)

  override def toStringMapped[T](mapping: Temp => T): String =
    s"""Block $label {
        |  ${tacs.map(_.toStringMapped(mapping)).mkString("\n  ")}
        |}""".stripMargin

  def self: BasicSsaBlock = this
}

case class SsaBlockPc(label: Label,
                      phis: List[Phi],
                      pcAfterPhi: ParallelCopy,
                      trailingTacs: IndexedSeq[Tac],
                      pcAtEnd: ParallelCopy)
  extends SsaBlock, ToStringMapped[Temp] {

  override def toStringMapped[B](mapping: Temp => B): String =
    s"""Block $label {
       |  ${phis.map(_.toStringMapped(mapping)).mkString("\n  ")}
       |  ${pcAfterPhi.toStringMapped(mapping)}
       |  ${trailingTacs.map(_.toStringMapped(mapping)).mkString("\n  ")}
       |  ${pcAtEnd.toStringMapped(mapping)}
       |}""".stripMargin
}