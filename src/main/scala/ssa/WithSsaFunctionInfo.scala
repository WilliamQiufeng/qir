package ssa

import common.FunctionIR

trait WithSsaFunctionInfo extends FunctionIR {
  def functionInfo: SsaFunctionInfo
}
