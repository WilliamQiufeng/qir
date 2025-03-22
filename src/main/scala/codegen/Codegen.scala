package codegen

import dag.{FunctionInfo, WithFunctionInfo}
import mem.{Architecture, CallStack, Frame, TempLocation}
import semantic.Temp
import ssa.{SsaFunctionInfo, WithSsaFunctionInfo}

import scala.collection.mutable

class Codegen(
               val withFunctionInfo: WithFunctionInfo,
               val architecture: Architecture
             ) extends WithFunctionInfo {

  private val tempLocations: mutable.Map[Temp, TempLocation] = mutable.Map.empty
  private val frame: Frame = new Frame {
    val frameLocals: mutable.ArrayBuffer[CallStack] = mutable.ArrayBuffer.empty[CallStack]

    override def locals: List[CallStack] = frameLocals.toList

    override def frameSize: Int = frameLocals.map(_.size).sum
  }

  override def functionInfo: FunctionInfo = withFunctionInfo.functionInfo

  {
    val argLocs = architecture.allocateArguments(functionInfo.header)
    tempLocations.addAll(functionInfo.header.arguments.zip(argLocs))
  }
}