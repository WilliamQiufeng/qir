import ast.*
import parser.lexer.*
import parser.lexer.implicits.implicitSymbol
import parsley.Parsley
import parsley.Parsley.{atomic, many, pure, some}
import parsley.combinator.{option, sepEndBy}
import parsley.expr.chain

package object parser {
  lazy val block: Parsley[Block] = Block(many(declaration), many(labelledBlock))
  lazy val program: Parsley[Program] = fully(Program(some(programUnit)))
  private lazy val valueType: Parsley[ValueType] =
    ("Unit" as TypeUnit)
      | ("Int" as TypeInt)
      | ("Char" as TypeChar)
      | ("Float" as TypeFloat)
      | TypeStruct(STRUCT_ID)
      | TypePointer("*" ~> valueType)
  private lazy val param: Parsley[Param] = Param(VAR_ID, ":" ~> valueType)
  private lazy val expr = AddInt("addi" ~> local, local) | SubInt("subi" ~> local, local) | MulInt("muli" ~> local, local) | DivInt("divi" ~> local, local)
    | Call("call" ~> labelValue, parenList(local))
    | arrayAccess
    | local
  private lazy val stmt =
    Assign(local, "=" ~> expr)
      | AssignElement(arrayAccess, "=" ~> local)
  private lazy val jump =
    Ret("ret" ~> local)
      | Branch("branch" ~> local, labelValue, labelValue)
      | Goto("goto" ~> labelValue)
  private lazy val declaration = Declaration("declare" ~> local, ":" ~> valueType)
  private lazy val labelledBlock = LabelledBlock(labelValue, ":" ~> many(stmt), jump)
  private lazy val programUnit =
    ConcreteFnDecl("fn" ~> labelValue, parenList(param), ":" ~> valueType, block)
      | ExternFnDecl("extern" ~> "fn" ~> labelValue, parenList(param), ":" ~> valueType)
      | ConstDecl("const" ~> VAR_ID, "=" ~> const)
      | StructDecl("struct" ~> STRUCT_ID, parenList(valueType))
  private val const: Parsley[Const] =
    atomic(ConstFloat(FLOAT))
      | ConstInteger(INTEGER)
      | ConstString(STRING)
      | ConstChar(CHAR)
      | ("undefined" as ConstUndefined)
  private val local: Parsley[Local] = Local(VAR_ID)
  private val labelValue: Parsley[LabelValue] = LabelValue(LABEL_ID)
  private val arrayAccess: Parsley[ArrayAccess] = ArrayAccess("[" ~> local <~ "]", Local(VAR_ID))

  private def parenList[T](arg: Parsley[T]) = "(" ~> sepEndBy(arg, ",") <~ ")"
}
