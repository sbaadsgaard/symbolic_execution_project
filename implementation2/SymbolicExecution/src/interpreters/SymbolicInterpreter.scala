package interpreters

import com.microsoft.z3
import com.microsoft.z3.{BoolExpr, Status}
import grammars.SymbolicGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.SymbolicGrammar.BOp.{Eq, Geq, Leq}
import grammars.SymbolicGrammar.Exp._
import grammars.SymbolicGrammar.SymbolicBool.{False, SymbolicBExp, True}
import grammars.SymbolicGrammar.SymbolicInt.{IntValue, Symbol, SymbolicAExp}
import grammars.SymbolicGrammar.SymbolicValue.{SymbolicBool, SymbolicInt}
import grammars.SymbolicGrammar._
import result.{Error, Ok, Result}

import scala.collection.immutable.HashMap


class SymbolicInterpreter(maxForks: Int = 10, ctx: z3.Context = new z3.Context()) {


  def interpExp(p: Prog, e: Exp, env: HashMap[Id, SymbolicValue], pc: PathConstraint, currForks: Int = 0): List[PathResult] = e match {
    case Lit(v) => List(PathResult(pc, Ok(v), env))
    case Var(id) => env.get(id) match {
      case None => List(PathResult(pc, Error(s"variable ${id.s} not defined"), env))
      case Some(value) => List(PathResult(pc, Ok(value), env))
    }
    case AExp(e1, e2, op) =>
      for {
        pr1 <- interpExp(p, e1, env, pc, currForks)
        pr2 <- interpExp(p, e2, pr1.env, pc, currForks)
      } yield PathResult(
        concatPathConstraints(pr1.pc, pr2.pc),
        pr1.res.flatMap(v1 => pr2.res.flatMap(v2 => (v1, v2) match {
          case (i: SymbolicInt, j: SymbolicInt) => Ok(SymbolicAExp(i, j, op))
          case _ => Error("arithmetic operation on non integer values")
        })),
        pr2.env
      )
    case BExp(e1, e2, op) =>
      for {
        pr1 <- interpExp(p, e1, env, pc, currForks)
        pr2 <- interpExp(p, e2, pr1.env, pc, currForks)
      } yield PathResult(
        concatPathConstraints(pr1.pc, pr2.pc),
        pr1.res.flatMap(v1 => pr2.res.flatMap(v2 => (v1, v2) match {
          case (i: SymbolicInt, j: SymbolicInt) => Ok(SymbolicBExp(i, j, op))
          case _ => Error("comparion of non integer values")
        })),
        pr2.env
      )

    case _ => List(PathResult(pc, Error("non implemented"), env))
  }

  def concatPathConstraints(pc1: PathConstraint, pc2: PathConstraint): PathConstraint = PathConstraint(pc1.conds ++ pc2.conds)

  def translateIntToZ3(v: SymbolicInt): z3.ArithExpr = v match {
    case IntValue(i) => ctx.mkInt(i)
    case Symbol(s) => ctx.mkIntConst(s)
    case SymbolicAExp(s1, s2, op) => op match {
      case Add() => ctx.mkAdd(translateIntToZ3(s1), translateIntToZ3(s2))
      case Sub() => ctx.mkSub(translateIntToZ3(s1), translateIntToZ3(s2))
      case Mul() => ctx.mkMul(translateIntToZ3(s1), translateIntToZ3(s2))
      case Div() => ctx.mkDiv(translateIntToZ3(s1), translateIntToZ3(s2))
    }
  }

  def translateBoolToZ3(b: SymbolicBool): BoolExpr = b match {
    case True() => ctx.mkTrue()
    case False() => ctx.mkFalse()
    case SymbolicBExp(i, j, op) => op match {
      case Leq() => ctx.mkLe(translateIntToZ3(i), translateIntToZ3(j))
      case Geq() => ctx.mkGe(translateIntToZ3(i), translateIntToZ3(j))
      case Eq() => ctx.mkEq(translateIntToZ3(i), translateIntToZ3(j))
    }
  }

  def checkSat(pc: PathConstraint): z3.Status = ctx.mkSolver().check(pc.conds.foldLeft(ctx.mkTrue())((z, b) => ctx.mkAnd(z, translateBoolToZ3(b))))
}


case class PathResult(pc: PathConstraint, res: Result[SymbolicValue, String], env: HashMap[Id, SymbolicValue])

case class PathConstraint(conds: List[SymbolicBool])




