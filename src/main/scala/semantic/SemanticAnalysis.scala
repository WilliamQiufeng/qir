package semantic

import cats.syntax.all.*
import ast.*

import scala.collection.mutable

class SemanticAnalysis(val program: Program) {
  val globalSymbolTable: GlobalSymbolTable = GlobalSymbolTable()
  val programUnitMap: Map[String, ProgramUnit] = Map.from(for decl <- program.lines yield decl match {
    case ConcreteFnDecl(name, _, _, _) => (name.name, decl)
    case ConstDecl(name, _) => (name, decl)
    case StructDecl(name, _) => (name, decl)
    case ExternFnDecl(name, _, _) => (name.name, decl)
  })
  private val constMap: mutable.HashMap[Const, ConstIRSymbol] = mutable.HashMap.empty

  def translateValueType(ty: ValueType): Either[String, Type] = translateValueType(ty, Set.empty)

  private def fillDeclaration(name: String, visited: Set[String]): Either[String, Type] = {
    if (visited.contains(name))
      return Left(name)
    for {
      n <- programUnitMap.get(name).toRight(s"Undeclared function/structure: $name")
      argDec <- fillDeclaration(n, visited.incl(name))
    }
    yield argDec.ty
  }

  def getOrAddConst(value: Const): ConstIRSymbol = constMap.getOrElseUpdate(value, ConstIRSymbol(value, Temp()))

  private def insertConst(name: String, value: Const) = {
    val constSymbol = ConstIRSymbol(value, Temp())
    constMap.getOrElseUpdate(value, constSymbol)
    globalSymbolTable.insert(name, constSymbol).asRight
  }

  private def translateValueType(ty: ValueType, visited: Set[String]): Either[String, Type] = ty match
    case TypePointer(element) => PointerType(() => translateValueType(element, visited).toOption.get).asRight
    case TypeStruct(name) => fillDeclaration(name, visited)
    case TypeUnit => UnitType.asRight
    case TypeInt => IntType.asRight
    case TypeChar => CharType.asRight
    case TypeFloat => FloatType.asRight

  private def fillFnDecl(name: String, args: List[Param], retTy: ValueType, visited: Set[String]) =
    globalSymbolTable.lookup(name).toRight(s"Function declaration for $name is not found") <+> (
      for {
        argTypes <- args.traverse(a => translateValueType(a.ty, visited))
        retType <- translateValueType(retTy, visited)
      } yield globalSymbolTable.insert(name, IRSymbol(Temp(), FunctionType(argTypes, retType))))

  private def fillDeclaration(programUnit: ProgramUnit, visited: Set[String]): Either[String, IRSymbol] = {
    programUnit match
      case ConcreteFnDecl(name, args, retTy, _) => fillFnDecl(name.name, args, retTy, visited)
      case ConstDecl(name, value) =>
        globalSymbolTable.lookup(name).toRight(s"Declaration of $name is not found") <+> insertConst(name, value)
      case StructDecl(name, members) => globalSymbolTable.lookup(name).toRight(s"Declaration of $name is not found") <+> (
        for {
          memberTypes <- members.traverse(a => translateValueType(a.ty, visited))
        } yield {
          val structSymbolTable = StructSymbolTable(members.map(_.name), memberTypes)
          globalSymbolTable.insert(name, IRSymbol(Temp(), StructType(memberTypes, structSymbolTable)))
        })
      case ExternFnDecl(name, args, retTy) => fillFnDecl(name.name, args, retTy, visited)
  }

  def fillDeclarations(): Either[String, Unit] = for _ <- program.lines.traverse(fillDeclaration(_, Set.empty)) yield ()
}
