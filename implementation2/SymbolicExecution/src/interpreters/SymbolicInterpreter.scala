package interpreters

import com.microsoft.z3
import com.microsoft.z3.{BoolExpr, Status}
import grammars.SymbolicGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.SymbolicGrammar.BOp.{Eq, Geq, Leq}
import grammars.SymbolicGrammar.Exp._
import grammars.SymbolicGrammar.SymbolicBool.{False, Not, SymbolicBExp, True}
import grammars.SymbolicGrammar.SymbolicInt.{IntValue, Symbol, SymbolicAExp}
import grammars.SymbolicGrammar.SymbolicValue.{SymbolicBool, SymbolicInt}
import grammars.SymbolicGrammar._
import interpreters.PathStatus.{Certain, Unknown}
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
        pr1.pc ++ pr2.pc,
        pr1.res.flatMap(v1 => pr2.res.flatMap(v2 => (v1, v2) match {
          case (i: IntValue, j: IntValue) => evalInts(i.v, j.v, op)
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
        pr1.pc ++ pr2.pc,
        pr1.res.flatMap(v1 => pr2.res.flatMap(v2 => (v1, v2) match {
          case (i: SymbolicInt, j: SymbolicInt) => Ok(SymbolicBExp(i, j, op))
          case _ => Error("comparion of non integer values")
        })),
        pr2.env
      )

    case IfExp(c, thenExp, elseExp) =>
      val r = for {
        pr <- interpExp(p, c, env, pc, currForks)
      } yield
        pr.res match {
          case Ok(b: SymbolicBool) => b match {
            case True() => interpExp(p, thenExp, pr.env, pr.pc, currForks)
            case False() => interpExp(p, elseExp, pr.env, pr.pc, currForks)
            case b =>
              val trueBranch = checkSat(pc, b)
              val falseBranch = checkSat(pc, Not(b))
              (trueBranch, falseBranch) match {
                case (Status.SATISFIABLE, Status.UNSATISFIABLE) =>
                  interpExp(p, thenExp, pr.env, pr.pc :+ b)
                case (Status.UNSATISFIABLE, Status.SATISFIABLE) =>
                  interpExp(p, elseExp, pr.env, pr.pc :+ Not(b))
                case (Status.SATISFIABLE, Status.SATISFIABLE) =>
                  interpExp(p, thenExp, pr.env, pr.pc :+ b) ++ interpExp(p, elseExp, pr.env, pr.pc :+ b)
                case (Status.UNKNOWN, Status.SATISFIABLE) =>
                  interpExp(p, thenExp, pr.env, pc :+ b unknown()) ++ interpExp(p, elseExp, pr.env, pr.pc :+ Not(b))
                case (Status.UNKNOWN, Status.UNSATISFIABLE) =>
                  interpExp(p, thenExp, pr.env, pc :+ b unknown())
                case (Status.SATISFIABLE, Status.UNKNOWN) =>
                  interpExp(p, thenExp, pr.env, pr.pc :+ b) ++ interpExp(p, elseExp, pr.env, pr.pc :+ b unknown())
                case (Status.UNSATISFIABLE, Status.UNKNOWN) =>
                  interpExp(p, elseExp, pr.env, pr.pc :+ Not(b) unknown())
                case (Status.UNKNOWN, Status.UNKNOWN) =>
                  interpExp(p, thenExp, pr.env, pr.pc :+ b unknown()) ++ interpExp(p, elseExp, pr.env, pr.pc :+ Not(b) unknown())
              }
          }
          case Ok(_) => List(PathResult(pr.pc, Error("non boolean condition in if expression"), pr.env))
          case _ => List(pr) // the condition evaluated to an error, so we just return this same error
        }
      r.flatten
    case _ => List(PathResult(pc, Error("non implemented"), env))
  }

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
    case Not(bool) => ctx.mkNot(translateBoolToZ3(bool))
  }

  def checkSat(pc: PathConstraint, b: SymbolicBool): z3.Status = {
    val z3Expr = ctx.mkAnd(pc.conds.foldLeft(ctx.mkTrue())((z, v) => ctx.mkAnd(z, translateBoolToZ3(v))), translateBoolToZ3(b))
    ctx.mkSolver().check(z3Expr)
  }

  def evalInts(i: Int, j: Int, op: AOp): Result[SymbolicValue, String] =  op match {
    case Add() => Ok(IntValue(i + j))
    case Sub() => Ok(IntValue(i - j))
    case Mul() => Ok(IntValue( i * j))
    case Div() if j == 0 => Error(s"division by zero")
    case Div() => Ok(IntValue(i / j))
  }
}


case class PathResult(pc: PathConstraint, res: Result[SymbolicValue, String], env: HashMap[Id, SymbolicValue])

case class PathConstraint(conds: List[SymbolicBool], ps: PathStatus = Certain()) {
  def ++(other: PathConstraint): PathConstraint = PathConstraint(this.conds ++ other.conds, this.ps.combine(other.ps))

  def :+(b: SymbolicBool): PathConstraint = PathConstraint(this.conds :+ b, this.ps)

  def unknown(): PathConstraint = PathConstraint(this.conds, Unknown())
}

sealed trait PathStatus {
  def combine(other: PathStatus): PathStatus = this match {
    case Unknown() => this
    case Certain() => other
  }
}

object PathStatus {

  case class Certain() extends PathStatus

  case class Unknown() extends PathStatus

}



