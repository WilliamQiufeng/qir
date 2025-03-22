package dag

import mem.{CallingConvention, TempLocation}
import semantic.{IRSymbol, Temp}

case class FunctionHeader(
                           callingConvention: CallingConvention,
                           arguments: List[Temp]
                         ) {
}
