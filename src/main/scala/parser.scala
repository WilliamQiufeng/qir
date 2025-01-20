import ast.*
import lexer.*
import lexer.implicits.implicitSymbol
import parsley.Parsley
import parsley.Parsley.{atomic, many, pure, some}
import parsley.combinator.{option, sepEndBy}
import parsley.expr.chain

object parser {
  lazy val member: Parsley[Member] = Member(VAR_ID, ":" ~> valueType)
  lazy val block: Parsley[Block] = Block(many(labelledBlock))
  lazy val program: Parsley[Program] = fully(Program(some(programUnit)))
  private lazy val valueType: Parsley[ValueType] =
    ("Unit" as TypeUnit)
      | ("Int" as TypeInt)
      | ("Char" as TypeChar)
      | ("Float" as TypeFloat)
      | TypeStruct(STRUCT_ID)
      | TypePointer("*" ~> valueType)
  private lazy val param: Parsley[Param] = Param(VAR_ID, ":" ~> valueType)
  private lazy val expr = AddInt("addi" ~> atom, atom) | SubInt("subi" ~> atom, atom) | MulInt("muli" ~> atom, atom) | DivInt("divi" ~> atom, atom)
    | Call("call" ~> labelValue, parenList(atom))
    | arrayAccess
    | atom
  private lazy val stmt =
    Assign(lvalue, option(":" ~> valueType), "=" ~> expr)
      | AssignElement(arrayAccess, "=" ~> atom)
      | Ret("ret" ~> (atom <|> pure(ConstUnit)))
      | Branch("branch" ~> atom, labelValue, labelValue)
  private lazy val labelledBlock = LabelledBlock(labelValue, ":" ~> many(stmt))
  private lazy val programUnit =
    ConcreteFnDecl("fn" ~> labelValue, parenList(param), ":" ~> valueType, block)
      | ExternFnDecl("extern" ~> "fn" ~> labelValue, parenList(param), ":" ~> valueType)
      | ConstDecl("const" ~> lvalue, "=" ~> const)
      | StructDecl("struct" ~> STRUCT_ID, parenList(member))
  private val const: Parsley[Const] = atomic(ConstFloat(FLOAT)) | ConstInteger(INTEGER) | ConstString(STRING) | ConstChar(CHAR)
  private val lvalue: Parsley[LValue] = chain.postfix(Var(VAR_ID), ("." ~> VAR_ID).map(n => ConstFieldAccess(_, n)))
  private val labelValue: Parsley[LabelValue] = LabelValue(LABEL_ID)
  private val atom: Parsley[Atom] = lvalue | const
  private val arrayAccess: Parsley[ArrayAccess] = ArrayAccess("[" ~> atom <~ "]", Var(VAR_ID))

  private def parenList[T](arg: Parsley[T]) = "(" ~> sepEndBy(arg, ",") <~ ")"
}
