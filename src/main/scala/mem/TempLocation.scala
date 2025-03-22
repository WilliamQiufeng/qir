package mem

import dag.FunctionHeader
import semantic.Temp

sealed trait TempLocation {
  val size: Int
}

trait Memory extends TempLocation {
  val address: Int
}

trait Register extends TempLocation {
  val calleeSave: Boolean
  val callerSave: Boolean
}

trait CallStack extends TempLocation {
  val offset: Int

  def offsetFromCaller: Int

  def offsetFromCallee: Int
}

trait StaticLink extends TempLocation {
  val innerLocation: TempLocation
}

trait Frame {
  def locals: List[CallStack]

  def frameSize: Int
}

trait Architecture {
  def registers: List[Register]

  def calleeSaveRegisters: List[Register]

  def callerSaveRegisters: List[Register]

  def stackPointer: Register

  def returnRegister: Register

  def allocateArguments(header: FunctionHeader): IndexedSeq[TempLocation]
}