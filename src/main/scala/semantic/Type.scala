package semantic

sealed trait Type {
  val size: Int
}

case object UnitType extends Type {
  override val size: Int = 0
}

case object IntType extends Type {
  override val size = 4
}

case object CharType extends Type {
  override val size = 1
}


case object FloatType extends Type {
  override val size = 8
}

case class PointerType(memberType: () => Type) extends Type {
  override val size: Int = 8
}

case class StructType(memberTypes: List[Type], symbolTable: StructSymbolTable) extends Type {
  override val size: Int = memberTypes.map(_.size).sum
}

case class ArrayType(memberType: Type, length: Int) extends Type {
  override val size: Int = memberType.size * length
}

case class FunctionType(argTypes: List[Type], retType: Type) extends Type {
  override val size: Int = 8
}