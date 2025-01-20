package tac

import semantic.Temp
import cats.syntax.all.*

trait Tac(var sources: Array[Temp], var definition: Option[Temp]) {
}

class Add(definition: Temp, arg1: Temp, arg2: Temp) extends Tac(Array(arg1, arg2), definition.some)

class Sub(definition: Temp, arg1: Temp, arg2: Temp) extends Tac(Array(arg1, arg2), definition.some)

class Mul(definition: Temp, arg1: Temp, arg2: Temp) extends Tac(Array(arg1, arg2), definition.some)

class Div(definition: Temp, arg1: Temp, arg2: Temp) extends Tac(Array(arg1, arg2), definition.some)

class Move(definition: Temp, source: Temp) extends Tac(Array(source), definition.some)

class Goto(var label: Label) extends Tac(Array(), None)

class Jump(test: Temp, var label1: Label, var label2: Label) extends Tac(Array(test), None)

class Ret(returnSink: Temp, value: Temp) extends Tac(Array(value), returnSink.some)
