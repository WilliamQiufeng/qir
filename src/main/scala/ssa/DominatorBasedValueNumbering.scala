package ssa

import cats.Monad
import cats.data.{Reader, StateT}
import cats.mtl.{Local, Stateful}
import cats.syntax.all.{toFlatMapOps, toFunctorOps, toTraverseOps}
import common.{CompilerContext, FunctionPass, FunctionPassResult}
import semantic.Temp
import tac.*

case object DominatorBasedValueNumbering extends FunctionPass[WithSsaFunctionInfo, WithSsaFunctionInfo] {
  type ValueNumber = Temp
  type ValueNumbering = Map[CanonicalisedExpression, ValueNumber]
  type PartialReader[T] = Reader[ValueNumbering, T]
  type NumberingState[T] = StateT[PartialReader, NumberingStateObject, T]
  type NumberingStateObject = Map[Label, BlockRewriteInfo]

  def getOrKey(temp: Temp)(implicit map: Map[Temp, ValueNumber]) = map.getOrElse(temp, temp)

  def getByExpr(expr: Expression, default: => ValueNumber)(implicit map: Map[CanonicalisedExpression, ValueNumber]) = map.getOrElse(expr, default)

  def getByTemp(temp: Temp)(implicit map: ValueNumbering) = map.getOrElse(TempExpr(temp), temp)

  override def apply(in: WithSsaFunctionInfo)(implicit ctx: CompilerContext): FunctionPassResult[WithSsaFunctionInfo] = {
    val performance = perform[NumberingState](in.functionInfo.startBlock)(implicitly, implicitly, implicitly, in)
    val partialReader = performance.runS(Map.empty.withDefault(_ => BlockRewriteInfo(Map.empty, Map.empty)))
    val res: NumberingStateObject = partialReader.run(Map.empty)
    println(s"Numbering: $res")
    val newLabelMap = in.functionInfo.labelMap.map {
      case (label, block) => label -> {
        res.get(label) match
          case Some(tempMapping) => block.rewrite(label,
            block.phis.flatMap {
              case phi@Phi(definition, sources, blockLabels) => tempMapping.normalMap.get(definition) match
                case Some(_) => None
                case None => Some(phi.rewrite(IndexedSeq(definition), sources.zip(blockLabels).map {
                  case (src, blockLabel) => getOrKey(src)(tempMapping.phiMap(blockLabel))
                }))
            },
            block.trailingTacs.flatMap { tac =>
              if tac.definitions.forall(tempMapping.normalMap.contains) && tac.definitions.nonEmpty then None else {
                Some(tac.rewrite(tac.definitions.map(getOrKey(_)(tempMapping.normalMap)), tac.sources.map(getOrKey(_)(tempMapping.normalMap))))
              }
            }.toIndexedSeq
          )
          case None => block
      }
    }
    Right(in.updatedFunctionInfo(in.functionInfo.copy(labelMap = newLabelMap)))
  }

  def numberSingleBlock[F[_] : Monad](blockLabel: Label)
                                     (implicit S: Stateful[F, NumberingStateObject], R: Local[F, ValueNumbering], in: WithSsaFunctionInfo): F[ValueNumbering] = {
    for
      dominatorNumbering <- R.ask
      phiMap <- S.inspect(_(blockLabel).phiMap)
      _ = println(s"Numbering $blockLabel with $dominatorNumbering")
      block = in.functionInfo.labelMap(blockLabel)
    yield block.tacs.foldLeft(dominatorNumbering) { case (currentNumbering, inst) => findExpr(inst, currentNumbering, phiMap) match
      case Some(expr) =>
        val canon = canonicalise(expr)
        println(canon)
        inst match
          case tac.ParallelCopy(_) => currentNumbering
          case tac.Move(d, s) =>
            println(s"+ move $d -> ${currentNumbering.getOrElse(TempExpr(s), s)}")
            currentNumbering + (TempExpr(d) -> currentNumbering.getOrElse(TempExpr(s), s))
          case _ if inst.definitions.size == 1 =>
            val dst = inst.definitions.head
            if currentNumbering.contains(canon) then
              println(s"+ exists $dst -> ${currentNumbering(canon)}")
              currentNumbering + (TempExpr(dst) -> currentNumbering(canon))
            else
              println(s"+ new $canon -> $dst")
              currentNumbering + (canon -> dst)
          case _ => currentNumbering
      case None => currentNumbering
    }
  }

  def findExpr(inst: Tac, valueNumbering: ValueNumbering, phiMap: Map[Label, Map[Temp, ValueNumber]]): Option[Expression] = {
    implicit val numbering: ValueNumbering = valueNumbering
    inst match {
      case phi: Phi =>
          val incoming = phi.sources.zip(phi.blockLabels).map { case (src, blockLabel) => phiMap.getOrElse(blockLabel, Map.empty).getOrElse(src, src) }
          if incoming.forall(_ == incoming.head) then
            Some(TempExpr(incoming.head))
          else
            Some(PhiExpr(phi.pairs.map { case (k, v) => k -> getByTemp(v) }))
        case BinaryArith(_, l, r, op) => op match
          case BinaryArithOp.AddI => Some(Add(getByTemp(l), getByTemp(r)))
          case BinaryArithOp.SubI => Some(Sub(getByTemp(l), getByTemp(r)))
          case BinaryArithOp.MulI => Some(Mul(getByTemp(l), getByTemp(r)))
          case BinaryArithOp.DivI => Some(Div(getByTemp(l), getByTemp(r)))

        case tac.Move(d, s) => Some(TempExpr(getByTemp(s)))
        case _ => None
        }
    }


  def perform[F[_] : Monad](blockLabel: Label)
                           (implicit S: Stateful[F, NumberingStateObject], R: Local[F, ValueNumbering], in: WithSsaFunctionInfo): F[Unit] =
    for
      newNumbering <- numberSingleBlock(blockLabel)
      newTempNumberings = newNumbering.flatMap {
        case (TempExpr(t), v) => Some(t -> v)
        case _ => None
      }
      _ <- S.modify(s => s + (blockLabel -> s(blockLabel).copy(normalMap = newTempNumberings)))
      _ <- S.modify(s => s ++ in.functionInfo.flowGraph.get(blockLabel).diSuccessors.map {
        succ => succ.outer -> s(succ.outer).copy(phiMap = s(succ.outer).phiMap + (blockLabel -> newTempNumberings))
      })
      _ <- in.functionInfo.dominanceInfo.dominationTree.get(blockLabel).diSuccessors.map(_.outer).toList.traverse { next =>
        R.local(perform(next))(* => newNumbering)
      }
    yield ()

  sealed trait Expression

  case class Add(left: ValueNumber, right: ValueNumber) extends Expression

  case class Sub(left: ValueNumber, right: ValueNumber) extends Expression

  case class Mul(left: ValueNumber, right: ValueNumber) extends Expression

  case class Div(left: ValueNumber, right: ValueNumber) extends Expression

  case class And(left: ValueNumber, right: ValueNumber) extends Expression

  case class Or(left: ValueNumber, right: ValueNumber) extends Expression

  case class Not(valueNumber: ValueNumber) extends Expression

  case class TempExpr(temp: Temp) extends Expression

  case class PhiExpr(mapping: Map[Label, ValueNumber]) extends Expression

  case class BlockRewriteInfo(normalMap: Map[Temp, ValueNumber],
                              phiMap: Map[Label, Map[Temp, ValueNumber]])

  def canonicalise(expression: Expression): CanonicalisedExpression = expression match
    case Add(left, right) if left.id > right.id => Add(right, left)
    case Mul(left, right) if left.id > right.id => Mul(right, left)
    case And(left, right) if left.id > right.id => And(right, left)
    case Or(left, right) if left.id > right.id => Or(right, left)
    case _ => expression

  opaque type CanonicalisedExpression = Expression
}
