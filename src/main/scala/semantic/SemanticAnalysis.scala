package semantic

import ast.*
import cats.Monad
import cats.data.{State, StateT}
import cats.mtl.Handle.handleStateT
import cats.mtl.{Handle, Raise, Stateful}
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxOptionId}
import cats.syntax.all.{toFlatMapOps, toFunctorOps, catsSyntaxApplyOps, toTraverseOps, toFoldableOps}

private type PartialEither[T] = Either[SemanticError, T]
private type PartialState[T] = State[SemanticAnalysisInfo, T]
private type SemanticAnalysisState[A] = StateT[PartialEither, SemanticAnalysisInfo, A]

case class SemanticAnalysisInfo(globalSymbolTable: GlobalSymbolTable = GlobalSymbolTable(),
                                constMap: Map[Const, ConstIRSymbol] = Map.empty,
                                constTempMap: Map[Temp, ConstIRSymbol] = Map.empty,
                                programUnitMap: Map[String, ProgramUnit],
                                typeSymbolTable: TypeSymbolTable = TypeSymbolTable()) {
  def lookupValueType(valueType: ValueType): Option[Type] = valueType match
    case TypePointer(element) => PointerType(() => lookupValueType(element).get).some
    case TypeStruct(name) => typeSymbolTable.lookup(name).map(_.ty)
    case TypeUnit => UnitType.some
    case TypeInt => IntType.some
    case TypeChar => CharType.some
    case TypeFloat => FloatType.some

  def lookupConstType(const: Const): Option[Type] = {
    const match
      case ast.ConstInteger(_) => IntType.pure
      case ast.ConstFloat(_) => FloatType.pure
      case ast.ConstString(_) => PointerType(() => CharType).pure
      case ast.ConstChar(_) => CharType.pure
      case ast.ConstUnit => UnitType.pure
      case ast.ConstUndefined => UndefinedType.pure
  }
}

object SemanticAnalysis {

  private def getConstType[F[_] : Monad](const: Const)
                                        (implicit H: Handle[F, SemanticError], S: Stateful[F, SemanticAnalysisInfo]): F[Type] = {
    const match
      case ast.ConstInteger(_) => IntType.pure
      case ast.ConstFloat(_) => FloatType.pure
      case ast.ConstString(_) => PointerType(() => CharType).pure
      case ast.ConstChar(_) => CharType.pure
      case ast.ConstUnit => UnitType.pure
      case ast.ConstUndefined => UndefinedType.pure
  }

  private def fillDeclaration[F[_] : Monad](name: String, visited: Set[String])
                                           (implicit H: Handle[F, SemanticError], S: Stateful[F, SemanticAnalysisInfo]): F[Type] = {
    if (visited.contains(name))
      return H.raise(RecursiveReference(name))
    for {
      programUnitMap <- S.inspect(_.programUnitMap)
      n <- H.fromOption(programUnitMap.get(name))(UndeclaredSymbol(name))
      argDec <- fillUnitDeclaration(name, n, visited.incl(name))
    }
    yield argDec.ty
  }

  private def getOrAddConst[F[_] : Monad](value: Const, name: Option[String] = None)
                                         (implicit H: Handle[F, SemanticError], S: Stateful[F, SemanticAnalysisInfo]): F[ConstIRSymbol] =
    for
      maybeSymbol <- S.inspect(_.constMap.get(value))
      constTy <- getConstType(value)
      resultSymbol <- maybeSymbol match {
        case None =>
          val newConst = ConstIRSymbol(value, Temp(), constTy, name)
          S.modify(info => info.copy(constMap = info.constMap + (value -> newConst), constTempMap = info.constTempMap + (newConst.temp -> newConst))) *>
            S.monad.pure(newConst)
        case Some(v) => S.monad.pure(v)
      }
    yield resultSymbol

  private def getOrAddGlobal[F[_] : Monad](name: String, symbol: => IRSymbol)
                                          (implicit S: Stateful[F, SemanticAnalysisInfo]): F[IRSymbol] =
    for
      maybeSymbol <- S.inspect(_.globalSymbolTable.lookup(name))
      resultSymbol <- maybeSymbol match {
        case None =>
          S.modify(info => info.copy(globalSymbolTable = info.globalSymbolTable.insert(name, symbol))) *> S.monad.pure(symbol)
        case Some(v) => S.monad.pure(v)
      }
    yield resultSymbol

  private def getOrAddType[F[_] : Monad](name: String, symbol: => TypeIRSymbol)
                                        (implicit S: Stateful[F, SemanticAnalysisInfo]): F[IRSymbol] =
    for
      maybeSymbol <- S.inspect(_.typeSymbolTable.lookup(name))
      resultSymbol <- maybeSymbol match {
        case None =>
          S.modify(info => info.copy(typeSymbolTable = info.typeSymbolTable.insert(name, symbol))) *> S.monad.pure(symbol)
        case Some(v) => S.monad.pure(v)
      }
    yield resultSymbol

  private def insertConst[F[_] : Monad](name: String, value: Const)
                                       (implicit H: Handle[F, SemanticError], S: Stateful[F, SemanticAnalysisInfo]): F[IRSymbol] = {
    for
      constSymbol <- getOrAddConst(value, name.some)
      _ <- getOrAddGlobal(name, constSymbol)
    yield constSymbol
  }

  private def lookup[F[_] : Monad](name: String)
                                  (implicit E: Raise[F, SemanticError], S: Stateful[F, SemanticAnalysisInfo]): F[IRSymbol] =
    S.inspect(_.globalSymbolTable.lookup(name)).flatMap(E.fromOption(_)(UndeclaredSymbol(name)))


  private def translateValueType[F[_] : Monad](ty: ValueType, visited: Set[String])
                                              (implicit S: Stateful[F, SemanticAnalysisInfo], H: Handle[F, SemanticError]): F[Type] =
    ty match
      case TypePointer(element) =>
        for
          x <- S.get
        yield PointerType(() => translateValueType[SemanticAnalysisState](element, visited).runA(x).toOption.get)
      case TypeStruct(name) => fillDeclaration(name, visited)
      case TypeUnit => UnitType.pure
      case TypeInt => IntType.pure
      case TypeChar => CharType.pure
      case TypeFloat => FloatType.pure


  private def fillFnDecl[F[_] : Monad](name: String, args: List[Param], retTy: ValueType, visited: Set[String])
                                      (implicit H: Handle[F, SemanticError], S: Stateful[F, SemanticAnalysisInfo]): F[IRSymbol] =
    H.handleWith(lookup(name))(_ =>
      for {
        argTypes <- args.traverse(a => translateValueType(a.ty, visited))
        retType <- translateValueType(retTy, visited)
        symbol <- getOrAddGlobal(name, NormalIRSymbol(Temp(), FunctionType(argTypes, retType), name.some))
      } yield symbol)

  private def fillUnitDeclaration[F[_] : Monad](name: String, programUnit: ProgramUnit, visited: Set[String])
                                               (implicit S: Stateful[F, SemanticAnalysisInfo], H: Handle[F, SemanticError]): F[IRSymbol] = {
    programUnit match
      case ConcreteFnDecl(name, args, retTy, _) => fillFnDecl(name.name, args, retTy, visited)
      case ConstDecl(name, value) =>
        H.handleWith(lookup(name))(_ => insertConst(name, value))
      case StructDecl(name, members) =>
        H.handleWith(lookup(name))(_ =>
          for {
            memberTypes <- members.traverse(translateValueType(_, visited))
            structSymbolTable = StructSymbolTable.from(memberTypes)
            symbol <- getOrAddType(name, TypeIRSymbol(StructType(memberTypes, structSymbolTable), name.some))
          } yield symbol)
      case ExternFnDecl(name, args, retTy) =>
        fillFnDecl(name.name, args, retTy, visited)
  }

  private def fillDeclarations[F[_] : Monad](program: Program)
                                            (implicit H: Handle[F, SemanticError], S: Stateful[F, SemanticAnalysisInfo]): F[Unit] =
    getOrAddConst(ast.ConstUnit, Some("$unit"))
    getOrAddConst(ast.ConstUndefined, Some("$undefined"))
    program.lines.traverse_(fillUnitDeclaration("*PROGRAM*", _, Set.empty))

  def apply(program: Program): PartialEither[SemanticAnalysisInfo] = {
    val programUnitMap: Map[String, ProgramUnit] = Map.from(for decl <- program.lines yield decl match {
      case ConcreteFnDecl(name, _, _, _) => (name.name, decl)
      case ConstDecl(name, _) => (name, decl)
      case StructDecl(name, _) => (name, decl)
      case ExternFnDecl(name, _, _) => (name.name, decl)
    })
    val res = fillDeclarations[SemanticAnalysisState](program)
    res.runS(SemanticAnalysisInfo(programUnitMap = programUnitMap))
  }
}
