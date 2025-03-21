package dag

import mem.{CallingConvention, TempLocation}
import semantic.{IRSymbol, Temp}

case class FunctionHeader(
                           tempLocations: Map[Temp, TempLocation],
                           callingConvention: CallingConvention,
                           arguments: List[Temp]
                         ) {
}
