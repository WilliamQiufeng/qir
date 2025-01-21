package dag

import semantic.Type

sealed trait SemanticError

case class TypeMismatch(desiredType: Type, foundType: Type) extends SemanticError
