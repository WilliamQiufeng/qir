import ast.ConcreteFnDecl
import dag.FunctionDag
import parsley.Parsley
import parsley.syntax.character.{charLift, stringLift}
import semantic.SemanticAnalysis
import ssa.{FunctionSsaConstructor, FunctionSsaPass, SsaConstructionPass}
import tac.asDot


val hello: Parsley[Unit] = ('h' ~> ("ello" | "i") ~> " world!").void

@main
def hi(): Unit = {
  val f = io.Source.fromResource("testCfg32.qir")
  val l = try f.mkString finally f.close()
  println(l)
  val ast = parser.program.parse(l)
  println(ast)
  SemanticAnalysis(ast.get) match
    case Left(err) => println(err)
    case Right(info) => println(info)
      for (x <- info.programUnitMap.values) {
        println(x)
        x match
          case d: ConcreteFnDecl =>
            val res = FunctionDag(info, d)
            val functionInfo = res.makeInfo
            val ssaFunctionInfo = FunctionSsaConstructor(functionInfo).perform
            println(res.errors)
            println(res.symbolTable)
            println(res.graph.asDot(x => ssaFunctionInfo.labelMap(x).toStringMapped(ssaFunctionInfo.tempMap)))
            println(ssaFunctionInfo.tempMap)
          //            val ssa = FunctionSsaConstructor(functionInfo).perform
          //            println(ssa.flowGraph.asDot)
          //            res.symbolTable.values.foreach(s =>
          //              println(s"$s: uses:${s.uses}")
          //            )
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