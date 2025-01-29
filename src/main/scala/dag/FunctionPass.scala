package dag

import ast.{Atom, LabelValue, Local, ValueType}
import semantic.{IRSymbol, Type}
import tac.Label

import scala.language.implicitConversions

abstract class FunctionPass(private val functionInfo: FunctionInfo) {
  implicit def valueTypeToType(valueType: ValueType): Type =
    functionInfo.semanticAnalysis.translateValueType(valueType).toOption.get

  implicit def localToTemp(local: Local): IRSymbol = local.name

  implicit def atomToTemp(atom: Atom): IRSymbol = {
    atom match
      case const: ast.Const => functionInfo.semanticAnalysis.getOrAddConst(const)
      case local: Local => local
  }

  implicit def stringToTemp(name: String): IRSymbol = {
    functionInfo.symbolTable.lookup(name) match {
      case None => functionInfo.semanticAnalysis.globalSymbolTable.lookup(name) match {
        case None => throw new Exception(s"Local $name not found in $functionInfo.functionDecl")
        case Some(s) => s
      }
      case Some(s) => s
    }
  }

  implicit def lookupBlock(label: Label): Block = functionInfo.labelMap(label)

  implicit def lookupLabelValue(labelValue: LabelValue): Label = functionInfo.labelSymbolMap(labelValue)

  def perform: FunctionInfo
}