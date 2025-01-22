package semantic

sealed trait Type(repr: String) {
  val size: Int

  override def toString: String = repr
}

case object UnitType extends Type("Unit") {
  override val size: Int = 0
}

case object IntType extends Type("Int") {
  override val size = 4
}

case object CharType extends Type("Char") {
  override val size = 1
}


case object FloatType extends Type("Float") {
  override val size = 8
}

case class PointerType(private val memberTypeFinder: () => Type) extends Type("Pointer") {
  override val size: Int = 8
  lazy val memberType = memberTypeFinder()
}

case class StructType(memberTypes: List[Type], symbolTable: StructSymbolTable) extends Type(symbolTable.types.mkString("{", ", ", "}")) {
  override val size: Int = memberTypes.map(_.size).sum
}

case class ArrayType(memberType: Type, length: Int) extends Type(s"[$length]$memberType") {
  override val size: Int = memberType.size * length
}

case class FunctionType(argTypes: List[Type], retType: Type) extends Type(s"${argTypes.mkString("(", ", ", ")")}->$retType") {
  override val size: Int = 8
}