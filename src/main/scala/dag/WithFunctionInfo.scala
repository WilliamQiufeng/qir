package dag

import ast.{Atom, LabelValue, Local, ValueType}
import common.CompilerContext
import semantic.{ConstIRSymbol, IRSymbol, Temp, Type}
import tac.{Block, Label}

import scala.language.implicitConversions

trait WithFunctionInfo {
  val functionInfo: FunctionInfo
  implicit def valueTypeToType(valueType: ValueType)(implicit ctx: CompilerContext): Type =
    ctx.semanticAnalysisInfo.lookupValueType(valueType).get

  implicit def localToTemp(local: Local)(implicit ctx: CompilerContext): IRSymbol = local.name

  implicit def atomToTemp(atom: Atom)(implicit ctx: CompilerContext): IRSymbol = {
    atom match
      case const: ast.Const => ConstIRSymbol(const, Temp(), ctx.semanticAnalysisInfo.lookupConstType(const).get)
      case local: Local => local
  }

  implicit def stringToTemp(name: String)(implicit ctx: CompilerContext): IRSymbol = {
    functionInfo.symbolTable.lookup(name) match {
      case None => ctx.semanticAnalysisInfo.globalSymbolTable.lookup(name) match {
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