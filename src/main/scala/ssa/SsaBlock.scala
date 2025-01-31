package ssa

import semantic.Temp
import tac.{Label, Phi, Tac, TacImpl}

import scala.collection.SeqView

case class SsaBlock(label: Label, phis: List[Tac[Phi]], trailingTacs: IndexedSeq[Tac[TacImpl]]) {

  def tacs: SeqView[Tac[TacImpl]] = trailingTacs.view.prependedAll(phis.view.map(t => Tac(t.sources, t.definition, t.impl)))

  override def toString: String = s"Block $label${tacs.mkString("{\n  ", "\n  ", "\n}")}"

  def toStringMapped[T](mapping: Temp => T): String = s"Block $label${tacs.map(_.toStringNamed(mapping)).mkString("{\n  ", "\n  ", "\n}")}"

  def self: SsaBlock = this

  //  def fillDefUse(): Unit = {
  //    for tac <- tacs do
  //      tac.sources.foreach(s => s.uses = s.uses.incl(BlockTac(tac, this)))
  //  }
}