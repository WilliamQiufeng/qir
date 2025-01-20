import parsley.Parsley
import parsley.syntax.character.{charLift, stringLift}


val hello: Parsley[Unit] = ('h' ~> ("ello" | "i") ~> " world!").void

@main
def hi(): Unit = {
  val f = io.Source.fromResource("testProgram.qir")
  val l = try f.mkString finally f.close()
  println(l)
  for i <- 1 to 10 yield println(time(parser.program.parse(l)))
}

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}