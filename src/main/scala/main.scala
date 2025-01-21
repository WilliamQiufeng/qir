import ast.ConcreteFnDecl
import dag.FunctionDag
import parsley.Parsley
import parsley.syntax.character.{charLift, stringLift}
import semantic.SemanticAnalysis


val hello: Parsley[Unit] = ('h' ~> ("ello" | "i") ~> " world!").void

@main
def hi(): Unit = {
  val f = io.Source.fromResource("testCfg33.qir")
  val l = try f.mkString finally f.close()
  println(l)
  val ast = parser.program.parse(l)
  println(ast)
  val seman = SemanticAnalysis(ast.get)
  println(seman.fillDeclarations())
  println(seman.globalSymbolTable)
  println(seman.programUnitMap)
  for (x <- seman.programUnitMap.values) {
    println(x)
    x match
      case d: ConcreteFnDecl =>
        val res = FunctionDag(seman, d)
        println(res.toDot)
        println(res.errors)
      case _ =>
  }
}

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}