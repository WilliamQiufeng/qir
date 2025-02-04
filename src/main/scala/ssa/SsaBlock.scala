package ssa

import semantic.{ConstIRSymbol, SsaSymbol, Temp}
import tac.{Branch, Goto, Jump, Label, Phi, Ret, Tac, TacImpl}

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