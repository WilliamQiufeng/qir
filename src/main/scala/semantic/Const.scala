package semantic

trait Const[+T] {
  def value: T
  def boolean: Option[Boolean] = None
  def ty: Type
}

case class ConstInt(value: BigInt, bits: Int = 32) extends Const[BigInt] {
  override def boolean: Option[Boolean] = Some(value > 0)

  override def ty: Type = IntType(bits)
}

case class ConstChar(value: Int) extends Const[Int] {

  override def boolean: Option[Boolean] = Some(value > 0)

  override def ty: Type = IntType(32)
}

case object ConstUnit extends Const[Unit] {
  override def ty: Type = UnitType

  override def value: Unit = ()
}
case object ConstUndefined extends Const[Unit] {
  override def ty: Type = UndefinedType

  override def value: Unit = ()
}

//case class ConstStruct(value: IndexedSeq[Const[?]]) extends Const[IndexedSeq[Const[?]]] {
//  override def boolean: Option[Boolean] = None
//
//  override def ty: Type = {
//    val types = value.map(_.ty)
//    val structSymbolTable = StructSymbolTable()
//  }
//}