package ssa

import semantic.{ConstIRSymbol, SsaSymbol, Temp}
import tac.*
import util.ToStringMapped

import scala.collection.SeqView

case class SsaBlockTac(tac: Tac[TacImpl], label: Label) {
  override def toString: String = toStringMappedFullTac(x => x)

  def toStringMapped[T](mapping: Temp => T): String = s"(${tac.impl.toString} at $label)"

  def toStringMappedFullTac[T](mapping: Temp => T): String = s"(${tac.toStringMapped(mapping)} at $label)"
}

trait SsaBlock {
  def label: Label

  def phis: List[Tac[Phi]]

  def trailingTacs: IndexedSeq[Tac[TacImpl]]

  def getNextBlockIfEmpty(tempMap: Map[Temp, SsaSymbol]): Option[Label] = {
    if phis.nonEmpty || trailingTacs.size != 1 then
      return None
    trailingTacs.last.impl match
      case jump: Jump => jump match
        case Goto(label) => Some(label)
        case Branch(label1, label2) => tempMap(trailingTacs.last.sources.head) match
          case ConstIRSymbol(ast.ConstInteger(v), _, _, _) => Some(if v > 0 then label1 else label2)
          case _ => None
        case Ret => None
      case _ => throw new Exception("What")
  }
}

case class BasicSsaBlock(label: Label, phis: List[Tac[Phi]], trailingTacs: IndexedSeq[Tac[TacImpl]]) extends SsaBlock, ToStringMapped[Temp] {

  def tacs: SeqView[Tac[TacImpl]] = trailingTacs.view.prependedAll(phis.view.map(t => Tac(t.sources, t.definition, t.impl)))

  override def toStringMapped[T](mapping: Temp => T): String =
    s"""Block $label {
        |  ${tacs.map(_.toStringMapped(mapping)).mkString("\n  ")}
        |}""".stripMargin

  def self: BasicSsaBlock = this
}

case class SsaBlockPc(label: Label,
                      phis: List[Tac[Phi]],
                      pcAfterPhi: ParallelCopy,
                      trailingTacs: IndexedSeq[Tac[TacImpl]],
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