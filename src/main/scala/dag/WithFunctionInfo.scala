package dag


import scala.language.implicitConversions

trait WithFunctionInfo {
  val functionInfo: FunctionInfo
  //  implicit def valueTypeToType(valueType: ValueType)(implicit ctx: CompilerContext): Type =
  //    ctx.semanticAnalysisInfo.lookupValueType(valueType).get
  //
  //  implicit def localToTemp(local: Local)(implicit ctx: CompilerContext): IRSymbol = local.name
  //
  //  implicit def stringToTemp(name: String)(implicit ctx: CompilerContext): IRSymbol = {
  //    functionInfo.symbolTable.lookup(name) match {
  //      case None => ctx.semanticAnalysisInfo.globalSymbolTable.lookup(name) match {
  //        case None => throw new Exception(s"Local $name not found in $functionInfo.functionDecl")
  //        case Some(s) => s
  //      }
  //      case Some(s) => s
  //    }
  //  }
  //
  //  implicit def lookupBlock(label: Label): Block = functionInfo.labelMap(label)
  //
  //  implicit def lookupLabelValue(labelValue: LabelValue): Label = functionInfo.labelSymbolMap(labelValue)
  //
  //  def perform: FunctionInfo
}