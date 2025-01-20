import parsley.generic.*

object ast {
  sealed trait ProgramUnit

  sealed trait FnDecl extends ProgramUnit:
    val name: LabelValue
    val args: List[Param]
    val retTy: ValueType

  sealed trait ValueType

  sealed trait Expr

  sealed trait BinaryExpr extends Expr:
    val left: Atom
    val right: Atom

  sealed trait Stmt

  sealed trait LValue extends Expr, Atom

  sealed trait Atom extends Expr

  sealed trait Const extends Atom

  case class Program(lines: List[ProgramUnit])

  case class ConstDecl(name: LValue, value: Const) extends ProgramUnit

  case class ConcreteFnDecl(name: LabelValue, args: List[Param], retTy: ValueType, block: Block) extends FnDecl

  case class ExternFnDecl(name: LabelValue, args: List[Param], retTy: ValueType) extends FnDecl

  case class StructDecl(name: String, members: List[Member]) extends ProgramUnit

  case class Param(name: String, ty: ValueType)

  case class Member(name: String, ty: ValueType)

  case class Block(labelledBlocks: List[LabelledBlock])

  case class LabelValue(name: String)

  case class LabelledBlock(name: LabelValue, stmts: List[Stmt])

  case class TypePointer(element: ValueType) extends ValueType

  case class TypeStruct(name: String) extends ValueType

  case class AddInt(left: Atom, right: Atom) extends BinaryExpr

  case class SubInt(left: Atom, right: Atom) extends BinaryExpr

  case class MulInt(left: Atom, right: Atom) extends BinaryExpr

  case class DivInt(left: Atom, right: Atom) extends BinaryExpr

  case class Call(fn: LabelValue, args: List[Atom]) extends Expr

  case class Assign(dst: LValue, ty: Option[ValueType], src: Expr) extends Stmt

  case class AssignElement(dst: ArrayAccess, src: Atom) extends Stmt

  case class Ret(value: Atom) extends Stmt

  case class Branch(test: Atom, trueLabel: LabelValue, falseLabel: LabelValue) extends Stmt

  case class Var(name: String) extends LValue

  case class ArrayAccess(offset: Atom, from: LValue) extends Expr

  case class ConstFieldAccess(from: LValue, fieldName: String) extends LValue

  case class ConstInteger(value: BigInt) extends Const

  case class ConstFloat(value: BigDecimal) extends Const

  case class ConstString(value: String) extends Const

  case class ConstChar(value: Int) extends Const

  object Program extends ParserBridge1[List[ProgramUnit], Program]

  object ConstDecl extends ParserBridge2[LValue, Const, ProgramUnit]

  object ConcreteFnDecl extends ParserBridge4[LabelValue, List[Param], ValueType, Block, ProgramUnit]

  object ExternFnDecl extends ParserBridge3[LabelValue, List[Param], ValueType, ProgramUnit]

  object StructDecl extends ParserBridge2[String, List[Member], ProgramUnit]

  object Param extends ParserBridge2[String, ValueType, Param]

  object Member extends ParserBridge2[String, ValueType, Member]

  object Block extends ParserBridge1[List[LabelledBlock], Block]

  object LabelValue extends ParserBridge1[String, LabelValue]

  object LabelledBlock extends ParserBridge2[LabelValue, List[Stmt], LabelledBlock]

  case object TypeUnit extends ValueType with ParserBridge0[ValueType]

  case object TypeInt extends ValueType with ParserBridge0[ValueType]

  case object TypeChar extends ValueType with ParserBridge0[ValueType]

  case object TypeFloat extends ValueType with ParserBridge0[ValueType]

  object TypePointer extends ParserBridge1[ValueType, ValueType]

  object TypeStruct extends ParserBridge1[String, ValueType]

  object AddInt extends ParserBridge2[Atom, Atom, BinaryExpr]

  object SubInt extends ParserBridge2[Atom, Atom, BinaryExpr]

  object MulInt extends ParserBridge2[Atom, Atom, BinaryExpr]

  object DivInt extends ParserBridge2[Atom, Atom, BinaryExpr]

  object Call extends ParserBridge2[LabelValue, List[Atom], Expr]

  object Assign extends ParserBridge3[LValue, Option[ValueType], Expr, Stmt]

  object AssignElement extends ParserBridge2[ArrayAccess, Atom, Stmt]

  object Ret extends ParserBridge1[Atom, Stmt]

  object Branch extends ParserBridge3[Atom, LabelValue, LabelValue, Stmt]

  case object ConstUnit extends Const with ParserBridge0[Const]

  object Var extends ParserBridge1[String, LValue]

  object ArrayAccess extends ParserBridge2[Atom, LValue, ArrayAccess]

  object ConstInteger extends ParserBridge1[BigInt, Const]

  object ConstFloat extends ParserBridge1[BigDecimal, Const]

  object ConstString extends ParserBridge1[String, Const]

  object ConstChar extends ParserBridge1[Int, Const]
}
