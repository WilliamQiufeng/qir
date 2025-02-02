package ssa

import semantic.Temp
import tac.{Label, Phi, Tac, TacImpl}

import scala.collection.SeqView

case class SsaBlockTac(tac: Tac[TacImpl], label: Label) {
  override def toString: String = toStringMappedFullTac(x => x)
  def toStringMapped[T](mapping: Temp => T): String = s"(${tac.impl.toString} at $label)"
  def toStringMappedFullTac[T](mapping: Temp => T): String = s"(${tac.toStringNamed(mapping)} at $label)"
}

case class SsaBlock(label: Label, phis: List[Tac[Phi]], trailingTacs: IndexedSeq[Tac[TacImpl]]) {

  def tacs: SeqView[Tac[TacImpl]] = trailingTacs.view.prependedAll(phis.view.map(t => Tac(t.sources, t.definition, t.impl)))

  override def toString: String = s"Block $label${tacs.mkString("{\n  ", "\n  ", "\n}")}"

  def toStringMapped[T](mapping: Temp => T): String = s"Block $label${tacs.map(_.toStringNamed(mapping)).mkString("{\n  ", "\n  ", "\n}")}"

  def self: SsaBlock = this
}