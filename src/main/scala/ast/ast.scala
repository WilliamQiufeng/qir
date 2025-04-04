import common.FunctionIR
import parsley.generic.*

package object ast {
  sealed trait ProgramUnit

  sealed trait FnDecl extends ProgramUnit:
    val name: LabelValue
    val args: List[Param]
    val retTy: ValueType

  sealed trait ValueType

  sealed trait Expr

  sealed trait BinaryExpr extends Expr:
    val left: Local
    val right: Local

  sealed trait Stmt

  case class Local(name: String) extends Expr

  sealed trait Const(repr: String) {
    override def toString: String = repr
    def boolean: Option[Boolean] = None
  }

  case class Program(lines: List[ProgramUnit])

  case class ConstDecl(name: String, value: Const) extends ProgramUnit

  case class ConcreteFnDecl(name: LabelValue, args: List[Param], retTy: ValueType, block: Block) extends FnDecl, FunctionIR

  case class ExternFnDecl(name: LabelValue, args: List[Param], retTy: ValueType) extends FnDecl

  case class StructDecl(name: String, members: List[ValueType]) extends ProgramUnit

  case class Param(name: String, ty: ValueType)

  case class Block(declarations: List[FunctionNamedDeclaration], labelledBlocks: List[LabelledBlock])

  case class LabelValue(name: String)

  case class LabelledBlock(name: LabelValue, stmts: List[Stmt], jump: Jump)

  case class TypePointer(element: ValueType) extends ValueType

  case class TypeStruct(name: String) extends ValueType

  case class AddInt(left: Local, right: Local) extends BinaryExpr

  case class SubInt(left: Local, right: Local) extends BinaryExpr

  case class MulInt(left: Local, right: Local) extends BinaryExpr

  case class DivInt(left: Local, right: Local) extends BinaryExpr

  case class Call(fn: LabelValue, args: List[Local]) extends Expr

  case class Assign(dst: Local, src: Expr) extends Stmt

  case class AssignElement(dst: ArrayAccess, src: Local) extends Stmt

  sealed trait FunctionNamedDeclaration

  case class Declaration(local: Local, ty: ValueType) extends FunctionNamedDeclaration

  case class FunctionConstDecl(local: Local, const: Const) extends FunctionNamedDeclaration

  sealed trait Jump

  case class Ret(value: Local) extends Jump

  case class Goto(label: LabelValue) extends Jump

  case class Branch(test: Local, trueLabel: LabelValue, falseLabel: LabelValue) extends Jump

  case class ArrayAccess(offset: Local, from: Local) extends Expr

  //  case class ConstFieldAccess(from: Var, fieldName: String) extends Var

  case class ConstInteger(value: BigInt) extends Const(value.toString) {
    override def boolean: Option[Boolean] = Some(value > 0)
  }

  case class ConstFloat(value: BigDecimal) extends Const(value.toString)

  case class ConstString(value: String) extends Const(value)

  case class ConstChar(value: Int) extends Const(value.toString)

  case object ConstUndefined extends Const("⊥") with ParserBridge0[Const]

  object Program extends ParserBridge1[List[ProgramUnit], Program]

  object ConstDecl extends ParserBridge2[String, Const, ProgramUnit]

  object ConcreteFnDecl extends ParserBridge4[LabelValue, List[Param], ValueType, Block, ProgramUnit]

  object ExternFnDecl extends ParserBridge3[LabelValue, List[Param], ValueType, ProgramUnit]

  object StructDecl extends ParserBridge2[String, List[ValueType], ProgramUnit]

  object Param extends ParserBridge2[String, ValueType, Param]

  object Block extends ParserBridge2[List[FunctionNamedDeclaration], List[LabelledBlock], Block]

  object LabelValue extends ParserBridge1[String, LabelValue]

  object LabelledBlock extends ParserBridge3[LabelValue, List[Stmt], Jump, LabelledBlock]

  case object TypeUnit extends ValueType with ParserBridge0[ValueType]

  case object TypeInt extends ValueType with ParserBridge0[ValueType]

  case object TypeChar extends ValueType with ParserBridge0[ValueType]

  case object TypeFloat extends ValueType with ParserBridge0[ValueType]

  object TypePointer extends ParserBridge1[ValueType, ValueType]

  object TypeStruct extends ParserBridge1[String, ValueType]

  object AddInt extends ParserBridge2[Local, Local, BinaryExpr]

  object SubInt extends ParserBridge2[Local, Local, BinaryExpr]

  object MulInt extends ParserBridge2[Local, Local, BinaryExpr]

  object DivInt extends ParserBridge2[Local, Local, BinaryExpr]

  object Call extends ParserBridge2[LabelValue, List[Local], Expr]

  object Assign extends ParserBridge2[Local, Expr, Stmt]

  object AssignElement extends ParserBridge2[ArrayAccess, Local, Stmt]

  object Declaration extends ParserBridge2[Local, ValueType, FunctionNamedDeclaration]

  object FunctionConstDecl extends ParserBridge2[Local, Const, FunctionNamedDeclaration]

  object Ret extends ParserBridge1[Local, Jump]

  object Goto extends ParserBridge1[LabelValue, Jump]

  object Branch extends ParserBridge3[Local, LabelValue, LabelValue, Jump]

  case object ConstUnit extends Const("()") with ParserBridge0[Const]

  object Local extends ParserBridge1[String, Local]

  object ArrayAccess extends ParserBridge2[Local, Local, ArrayAccess]

  object ConstInteger extends ParserBridge1[BigInt, Const]

  object ConstFloat extends ParserBridge1[BigDecimal, Const]

  object ConstString extends ParserBridge1[String, Const]

  object ConstChar extends ParserBridge1[Int, Const]
}
