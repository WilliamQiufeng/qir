import parsley.Parsley
import parsley.syntax.character.{charLift, stringLift}
import semantic.SemanticAnalysis


val hello: Parsley[Unit] = ('h' ~> ("ello" | "i") ~> " world!").void

@main
def hi(): Unit = {
  val f = io.Source.fromResource("testProgram.qir")
  val l = try f.mkString finally f.close()
  println(l)
  val ast = parser.program.parse(l)
  println(ast)
  val seman = SemanticAnalysis(ast.get)
  println(seman.fillDeclarations())
  println(seman.globalSymbolTable)
}

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}