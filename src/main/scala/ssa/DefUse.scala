package ssa

import semantic.Temp

case class DefUse(definition: Option[SsaBlockTac], uses: List[SsaBlockTac]) {
  override def toString: String = s"Def: ${definition.mkString} Uses: ${uses.mkString(", ")}"
}
