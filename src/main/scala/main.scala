import algebra.lattice.Lattice
import ast.ConcreteFnDecl
import cats.implicits.catsSyntaxPartialOrder
import common.{CompilerContext, FunctionPassOps}
import dag.FunctionDagGenerationPass
import parsley.Parsley
import parsley.syntax.character.{charLift, stringLift}
import semantic.SemanticAnalysis
import ssa.{ConventionalizeSsaPass, DominatorBasedValueNumbering, RemoveRedundantBlockPass, SCCPPass, SsaConstructionPass}
import tac.asDot
import util.lattices.setBoundedLattice
import util.syntax.LatticeSyntax.MeetOps

val hello: Parsley[Unit] = ('h' ~> ("ello" | "i") ~> " world!").void

@main
def hi(): Unit = {
  val x = Set(0)
  val y = Set(1)
  println(x ^ y)
  {
    implicit val dual: Lattice[Set[Int]] = setBoundedLattice.dual
    println(x ^ y)
    println(x > y)
  }
  val f = io.Source.fromResource("testDBVN.qir")
  val l = try f.mkString finally f.close()
  println(l)
  val ast = parser.program.parse(l)
  println(ast)
  val pipeline = FunctionDagGenerationPass andThen SsaConstructionPass andThen DominatorBasedValueNumbering

  for i <- 0 to 0 do
    SemanticAnalysis(ast.get) match
      case Left(err) => println(err)
      case Right(info) => println(info)
        for (x <- info.programUnitMap.values) {
          println(x)
          x match
            case d: ConcreteFnDecl =>
              val res = time(pipeline(d)(CompilerContext(info)))
              res match
                case Left(value) => println(value)
                case Right(ssaFunctionInfo) =>
                  println(ssaFunctionInfo.functionInfo.symbolTable)
                  println(ssaFunctionInfo.functionInfo.labelMap)
                  println(ssaFunctionInfo.functionInfo.toDot)
                  println(ssaFunctionInfo.functionInfo.tempMap)
                  println("Def-Use: ")
                  println(ssaFunctionInfo.functionInfo.defUseToString)
                  println(ssaFunctionInfo.functionInfo.ssaGraph.asDot(_.toStringMapped(ssaFunctionInfo.functionInfo.tempMap)))
                  println(ssaFunctionInfo.functionInfo.dominanceInfo.dominationTree.asDot(ssaFunctionInfo.functionInfo.labelMap))
//                  val s = SparseConditionalConstantPropagation(ssaFunctionInfo).result
//                  println(s.value.mkString("\n"))
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