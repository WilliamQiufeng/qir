package semantic

import common.IRError

sealed trait SemanticError extends IRError

case class TypeMismatch(desiredType: Type, foundType: Type) extends SemanticError

case class UndeclaredSymbol(name: String) extends SemanticError

case class RecursiveReference(name: String) extends SemanticError

case object SomeSemanticError extends SemanticError