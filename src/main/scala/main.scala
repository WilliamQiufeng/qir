import ast.ConcreteFnDecl
import dag.FunctionDag
import parsley.Parsley
import parsley.syntax.character.{charLift, stringLift}
import semantic.SemanticAnalysis
import dag.toDot


val hello: Parsley[Unit] = ('h' ~> ("ello" | "i") ~> " world!").void

@main
def hi(): Unit = {
  val f = io.Source.fromResource("testCfg32.qir")
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
        val ssa = res.makeSsa
        println(ssa.flowGraph.toDot)
        println(res.errors)
        println(res.symbolTable)
        res.symbolTable.values.foreach(s =>
          println(s"$s: uses:${s.uses}")
        )
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