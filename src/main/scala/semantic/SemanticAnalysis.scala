package semantic

import cats.syntax.all._
import ast.*

class SemanticAnalysis(val program: Program) {
  val globalSymbolTable: GlobalSymbolTable = GlobalSymbolTable()
  private val programUnitMap: Map[String, ProgramUnit] = Map.newBuilder(for decl <- program.lines yield decl match {
    case ConcreteFnDecl(name, _, _, _) => (name, decl)
    case ConstDecl(name, _) => (name, decl)
    case StructDecl(name, _) => (name, decl)
    case ExternFnDecl(name, _, _) => (name, decl)
  }).result()

  private def fillDeclaration(name: String, visited: Set[String]): Either[String, Type] = {
    if (visited.contains(name))
      return Left(name)
    for {
      n <- programUnitMap.get(name).toRight(s"Undeclared function/structure: $name")
      argDec <- fillDeclaration(n, visited.incl(name))
    }
    yield argDec.ty
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
        globalSymbolTable.lookup(name).toRight(s"Declaration of $name is not found")
          <+> globalSymbolTable.insert(name, ConstIRSymbol(value, Temp())).asRight
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
