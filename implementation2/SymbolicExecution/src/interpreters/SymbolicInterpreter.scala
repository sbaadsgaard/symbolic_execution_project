package interpreters

import com.microsoft.z3
import com.microsoft.z3.{BoolExpr, Status}
import grammars.SymbolicGrammar
import grammars.SymbolicGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.SymbolicGrammar.BOp.{Eq, Geq, Leq}
import grammars.SymbolicGrammar.Exp._
import grammars.SymbolicGrammar.SymbolicBool.{False, Not, SymbolicBExp, True}
import grammars.SymbolicGrammar.SymbolicInt.{IntValue, Symbol, SymbolicAExp}
import grammars.SymbolicGrammar.SymbolicValue.{SymbolicBool, SymbolicInt, UnitValue}
import grammars.SymbolicGrammar._
import interpreters.PathStatus.{Certain, Unknown}
import result.{Error, Ok, Result}
import util.Util

import scala.collection.immutable.HashMap


class SymbolicInterpreter(maxForks: Int = 10, ctx: z3.Context = new z3.Context()) {

  val util = new Util(ctx)


  def interpExp(p: Prog, e: Exp, env: HashMap[Id, SymbolicValue], pc: PathConstraint, currForks: Int = 0): List[PathResult] = e match {
    case Lit(v) => List(PathResult(pc, Ok(v), env))
    case Var(id) => env.get(id) match {
      case None => List(PathResult(pc, Error(s"variable ${id.s} not defined"), env))
      case Some(value) => List(PathResult(pc, Ok(value), env))
    }
    case AExp(e1, e2, op) =>
      for {
        pr1 <- interpExp(p, e1, env, pc, currForks)
        pr2 <- interpExp(p, e2, pr1.env, pr1.pc, currForks)
      } yield
        PathResult(
          pr2.pc,
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
        pr2 <- interpExp(p, e2, pr1.env, pr1.pc, currForks)
      } yield
        PathResult(
          pr2.pc,
          pr1.res.flatMap(v1 => pr2.res.flatMap(v2 => (v1, v2) match {
            case (i: IntValue, j: IntValue) => compareInts(i.v, j.v, op)
            case (i: SymbolicInt, j: SymbolicInt) => Ok(SymbolicBExp(i, j, op))
            case _ => Error("comparison of non integer values")
          })),
          pr2.env
        )

    case AssignExp(variable, exp) => handleAssignment(p, exp, variable.id, env, pc)

    case IfExp(c, thenExp, elseExp) =>
      val r = for {
        pr <- interpExp(p, c, env, pc, currForks)
      } yield
        pr.res match {
          case Ok(bool: SymbolicBool) => bool match {
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
                  interpExp(p, thenExp, pr.env, pr.pc :+ b) ++ interpExp(p, elseExp, pr.env, pr.pc :+ Not(b))
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


    case wexp: WhileExp =>
      val r = for {
        pr <- interpExp(p, wexp.c, env, pc, currForks)
      } yield {
        pr.res match {
          case Ok(bool: SymbolicBool) => bool match {
            case True() => interpExp(p, SeqExp(wexp.doExp, wexp), pr.env, pr.pc, currForks)
            case False() => List(PathResult(pr.pc, Ok(UnitValue()), pr.env))
            case b =>
              val trueBranch = checkSat(pr.pc, b)
              val falseBranch = checkSat(pr.pc, Not(b))
              (trueBranch, falseBranch) match {
                case (Status.SATISFIABLE, Status.UNSATISFIABLE) =>
                  interpExp(p, SeqExp(wexp.doExp, wexp), pr.env, pr.pc :+ b, currForks)
                case (Status.UNSATISFIABLE, Status.SATISFIABLE) =>
                  List(PathResult(pc :+ Not(b), Ok(UnitValue()), pr.env))
                case (Status.SATISFIABLE, Status.SATISFIABLE) =>
                  interpExp(p, SeqExp(wexp.doExp, wexp), pr.env, pr.pc :+ b, currForks) :+ PathResult(pr.pc :+ Not(b), Ok(UnitValue()), pr.env)
                case (Status.UNKNOWN, Status.SATISFIABLE) =>
                  interpExp(p, SeqExp(wexp.doExp, wexp), pr.env, pr.pc :+ b unknown(), currForks) :+ PathResult(pr.pc :+ Not(b), Ok(UnitValue()), pr.env)
                case (Status.UNKNOWN, Status.UNSATISFIABLE) =>
                  interpExp(p, SeqExp(wexp.doExp, wexp), pr.env, pr.pc :+ b unknown(), currForks)
                case (Status.SATISFIABLE, Status.UNKNOWN) =>
                  interpExp(p, SeqExp(wexp.doExp, wexp), pr.env, pr.pc :+ b, currForks) :+ PathResult(pr.pc :+ Not(b) unknown(), Ok(UnitValue()), pr.env)
                case (Status.UNSATISFIABLE, Status.UNKNOWN) =>
                  List(PathResult(pr.pc :+ Not(b) unknown(), Ok(UnitValue()), pr.env))
                case (Status.UNKNOWN, Status.UNKNOWN) =>
                  interpExp(p, SeqExp(wexp.doExp, wexp), pr.env, pr.pc :+ b unknown(), currForks) :+ PathResult(pr.pc :+ Not(b) unknown(), Ok(UnitValue()), pr.env)
              }
          }
          case Ok(_) => List(PathResult(pr.pc, Error("non boolean condition in while expression"), pr.env))
          case _ => List(pr)
        }
      }
      r.flatten
    // So far i make a restriction that Arguments to functions can only be literals. If we allow general expressions i have to do some serious magic.
    // if functions are side effect free, how about pc? which should be returned
    case CallExp(id, args) => p.funcs.get(id.s) match {
      case None => List(PathResult(pc, Error(s"function ${id.s} not defined"), env))
      case Some(f) if args.length != f.params.length => List(PathResult(pc, Error("formal and actual parameter list differ in length"), env))
      case Some(f) =>
        Result.traverse(args)({
          case Lit(v: UnitValue) => Error("unit value as argument to function")
          case Lit(v) => Ok(v)
        }).map(l => l.zip(f.params).foldLeft(env)((nextEnv, t) => nextEnv + (t._2 -> t._1))) match {
          case Ok(localEnv) => for {
            pr <- interpExp(p, f.body, localEnv, pc)
          } yield PathResult(pr.pc, pr.res, env)
          case Error(msg) => List(PathResult(pc, Error(msg), env))
        }
    }

    case SeqExp(e1, e2) => for {
      pr1 <- interpExp(p, e1, env, pc, currForks)
      pr2 <- interpExp(p, e2, pr1.env, pr1.pc, currForks)
    } yield PathResult(
      pr2.pc,
      pr1.res.flatMap(_ => pr2.res.map(v => v)),
      pr2.env
    )
    case _ => List(PathResult(pc, Error("non implemented"), env))
  }

  def checkSat(pc: PathConstraint, b: SymbolicBool): z3.Status = {
    val z3Expr = ctx.mkAnd(pc.conds.foldLeft(ctx.mkTrue())((z, v) => ctx.mkAnd(z, util.translateBoolToZ3(v))), util.translateBoolToZ3(b))
    ctx.mkSolver().check(z3Expr)
  }

  def evalInts(i: Int, j: Int, op: AOp): Result[SymbolicValue, String] = op match {
    case Add() => Ok(IntValue(i + j))
    case Sub() => Ok(IntValue(i - j))
    case Mul() => Ok(IntValue(i * j))
    case Div() if j == 0 => Error(s"division by zero")
    case Div() => Ok(IntValue(i / j))
  }

  def compareInts(i: Int, j: Int, op: SymbolicGrammar.BOp): Result[SymbolicValue, String] = op match {
    case Leq() => if (i <= j) Ok(True()) else Ok(False())
    case Geq() => if (i >= j) Ok(True()) else Ok(False())
    case Eq() => if (i == j) Ok(True()) else Ok(False())
  }

  def handleAssignment(p: Prog, e: Exp, id: Id, env: HashMap[Id, SymbolicValue], pc: PathConstraint): List[PathResult] =
    for {
      pr <- interpExp(p, e, env, pc)
    } yield pr.res match {
      //should this be the original env? what about x := (y:= 3; unitValue), should the y binding still exist?
      case Ok(_: UnitValue) => PathResult(pr.pc, Error(s"Assignment of unitvalue to variable ${id.s}"), pr.env)
      case Ok(v) => PathResult(pr.pc, Ok(v), pr.env + (id -> v))
      case Error(_) => pr //Evaluation returned in an error, so we just pass this on
    }
}


//should i make this generic in a separate file?

case class PathResult(pc: PathConstraint, res: Result[SymbolicValue, String], env: HashMap[Id, SymbolicValue])

case class PathConstraint(conds: List[SymbolicBool] = List.empty[SymbolicBool], ps: PathStatus = Certain()) {
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



