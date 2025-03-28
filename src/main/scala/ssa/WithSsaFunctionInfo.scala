package ssa

import common.FunctionIR
import dag.FunctionInfo

trait WithSsaFunctionInfo extends FunctionIR {
  def functionInfo: SsaFunctionInfo
  def updatedFunctionInfo(newInfo: SsaFunctionInfo): WithSsaFunctionInfo
}
