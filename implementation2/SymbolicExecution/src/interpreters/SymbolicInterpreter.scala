package interpreters

import com.microsoft.z3
import com.microsoft.z3.Status
import grammars.SymbolicGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.SymbolicGrammar.BOp.{Eq, Geq, Leq}
import grammars.SymbolicGrammar.Exp._
import grammars.SymbolicGrammar.Stm.{AssignStm, IfStm, SeqStm}
import grammars.SymbolicGrammar.SymbolicBool.{False, Not, SymbolicBExp, True}
import grammars.SymbolicGrammar.SymbolicInt.{IntValue, SymbolicAExp}
import grammars.SymbolicGrammar.SymbolicValue.{SymbolicBool, SymbolicInt, UnitValue}
import grammars.SymbolicGrammar._
import interpreters.PathStatus.{Certain, Unknown}
import result.{Error, Ok, Result}
import util.Util

import scala.collection.immutable.HashMap


class SymbolicInterpreter(maxForks: Int = 10, ctx: z3.Context = new z3.Context()) {

  def interpProg(p: Prog): List[ExpRes] = interpExp(p, p.fCall, HashMap[Id, SymbolicValue](), PathConstraint())

  def interpStm(p: Prog, stm: Stm, env: HashMap[Id, SymbolicValue], pc: PathConstraint): List[StmRes] = stm match {
    case AssignStm(variable, e) =>
      for {
        expRes <- interpExp(p, e, env, pc)
      } yield expRes.res match {
        case Ok(UnitValue()) => StmRes(Error(s"assignment of unit value to ${variable.id.s}"), env, expRes.pc)
        case Ok(v) => StmRes(Ok(v), env + (variable.id -> v), expRes.pc)
        case err: Error[String] => StmRes(err, env, expRes.pc)
      }
    case IfStm(cond, thenStm, elsStm) => for {
      expRes: ExpRes <- interpExp(p, cond, env, pc)
      r <- expRes.res match {
        case Ok(bool: SymbolicBool) => bool match {
          case True() => interpStm(p, thenStm, env, expRes.pc)
          case False() => interpStm(p, elsStm, env, expRes.pc)
          case b =>
            val trueBranch = Util.checkSat(pc, b)
            val falseBranch = Util.checkSat(pc, Not(b))
            (trueBranch, falseBranch) match {
              case (Status.SATISFIABLE, Status.UNSATISFIABLE) =>
                interpStm(p, thenStm, env, expRes.pc :+ b)
              case (Status.UNSATISFIABLE, Status.SATISFIABLE) =>
                interpStm(p, elsStm, env, expRes.pc :+ Not(b))
              case (Status.SATISFIABLE, Status.SATISFIABLE) =>
                interpStm(p, thenStm, env, expRes.pc :+ b) ++ interpStm(p, elsStm, env, expRes.pc :+ Not(b))
              case (Status.UNKNOWN, Status.SATISFIABLE) =>
                interpStm(p, thenStm, env, expRes.pc :+ b unknown()) ++ interpStm(p, elsStm, env, expRes.pc :+ Not(b))
              case (Status.UNKNOWN, Status.UNSATISFIABLE) =>
                interpStm(p, thenStm, env, expRes.pc :+ b unknown())
              case (Status.SATISFIABLE, Status.UNKNOWN) =>
                interpStm(p, thenStm, env, expRes.pc :+ b) ++ interpStm(p, elsStm, env, expRes.pc :+ b unknown())
              case (Status.UNSATISFIABLE, Status.UNKNOWN) =>
                interpStm(p, elsStm, env, expRes.pc :+ Not(b) unknown())
              case (Status.UNKNOWN, Status.UNKNOWN) =>
                interpStm(p, thenStm, env, pc :+ b unknown()) ++ interpStm(p, elsStm, env, expRes.pc :+ Not(b) unknown())
              case _ => List(StmRes(Error("both paths in if statement is unsatisfiable"), env, expRes.pc))
            }
        }
        case Ok(_) => List(StmRes(Error("non boolean value as condition in if statement"), env, expRes.pc))
        case err: Error[String] => List(StmRes(err, env, expRes.pc))
      }
    } yield r

    case SeqStm(s1, s2) =>
      for {
        r1 <- interpStm(p, s1, env, pc)
        r2 <- interpStm(p, s2, r1.env, r1.pc)
      } yield StmRes(
        r1.res.flatMap(v1 => r2.res.map(v2 => v2)),
        r2.env,
        r2.pc
      )
    case _ => List(StmRes(Error("not implemented"), env, pc))
  }


  def interpExp(p: Prog, e: Exp, env: HashMap[Id, SymbolicValue], pc: PathConstraint): List[ExpRes] = e match {

    case Lit(v) => List(ExpRes(Ok(v), pc))

    case Var(id) => env.get(id) match {
      case None => List(ExpRes(Error(s"variable ${id.s} not defined"), pc))
      case Some(value) => List(ExpRes(Ok(value), pc))
    }

    case AExp(e1, e2, op) =>
      for {
        r1 <- interpExp(p, e1, env, pc)
        r2 <- interpExp(p, e2, env, r1.pc)
      } yield ExpRes(
        r1.res.flatMap(v1 => r2.res.flatMap(v2 => (v1, v2) match {
          case (i: IntValue, j: IntValue) => evalInts(i.v, j.v, op)
          case (i: SymbolicInt, j: SymbolicInt) => Ok(SymbolicAExp(i, j, op))
          case _ => Error("arithmetic operations on non integer values")
        })),
        r2.pc
      )

    case BExp(e1, e2, op) =>
      for {
        r1 <- interpExp(p, e1, env, pc)
        r2 <- interpExp(p, e2, env, r1.pc)
      } yield ExpRes(
        r1.res.flatMap(v1 => r2.res.flatMap(v2 => (v1, v2) match {
          case (i: IntValue, j: IntValue) => compareInts(i.v, j.v, op)
          case (i: SymbolicInt, j: SymbolicInt) => Ok(SymbolicBExp(i, j, op))
          case _ => Error("comparison of non integer values")
        })),
        r2.pc
      )

    case CallExp(id, args) => p.funcs.get(id) match {
      case None => List(ExpRes(Error(s"function ${id.s} not defined"), pc))
      case Some(f) if args.length != f.params.length => List(ExpRes(Error(s"formal and actual parameter list differ in length"), pc))
      case Some(f) =>
        //https://stackoverflow.com/a/44496231/11075156 sådan fik jeg cartesian product.
        val argsEvaled = args.map(interpExp(p, _, env, pc)) // this is a list that contains a list for each argument, that contains all possible values for the given argument
      val possibleArgLists = //take the cartesian product over these lists, to get a list of possible argument lists
        argsEvaled.foldRight(List[List[ExpRes]](Nil))((e1, rest) => e1.flatMap(p => rest.map(p :: _)))
        for {
          argList <- possibleArgLists
          r <- argList.zip(f.params).foldLeft[Result[HashMap[Id, SymbolicValue], String]](Ok(env))((next, expResIdPair) => next.flatMap(buildEnv(_, expResIdPair._1.res, expResIdPair._2))) match {
            // the path constraint is the combined path constraint of each evaluation of the arguments
            case err: Error[String] => List(ExpRes(Error(s"formal and actual parameter list differ in length"), argList.foldLeft[PathConstraint](pc)((next, expRes) => next ++ expRes.pc)))
            case Ok(localEnv) => evalFuncBody(p, f.body, localEnv, argList.foldLeft[PathConstraint](pc)((next, expRes) => next ++ expRes.pc))
          }
        } yield r 
    }
  }

  def evalFuncBody(p: Prog, body: FBody, env: HashMap[Id, SymbolicValue], pc: PathConstraint): List[ExpRes] =
    for {
      r <- interpStm(p, body.stm, env, pc)
      e <- r.res match {
        case err: Error[String] => List(ExpRes(err, r.pc))
        case Ok(_) => interpExp(p, body.returnExp, r.env, r.pc)
      }
    } yield e

  def buildEnv(env: HashMap[Id, SymbolicValue], v: Result[SymbolicValue, String], id: Id): Result[HashMap[Id, SymbolicValue], String] = v.flatMap({
    case UnitValue() => Error("unit value as argument to function call")
    case value => Ok(env + (id -> value))
  })


  def evalInts(i: Int, j: Int, op: AOp): Result[SymbolicValue, String] = op match {
    case Add() => Ok(IntValue(i + j))
    case Sub() => Ok(IntValue(i - j))
    case Mul() => Ok(IntValue(i * j))
    case Div() if j == 0 => Error(s"division by zero")
    case Div() => Ok(IntValue(i / j))
  }


  def compareInts(i: Int, j: Int, op: BOp): Result[SymbolicValue, String] = op match {
    case Leq() => if (i <= j) Ok(True()) else Ok(False())
    case Geq() => if (i >= j) Ok(True()) else Ok(False())
    case Eq() => if (i == j) Ok(True()) else Ok(False())
  }
}

case class ExpRes(res: Result[SymbolicValue, String], pc: PathConstraint)

case class StmRes(res: Result[SymbolicValue, String], env: HashMap[Id, SymbolicValue], pc: PathConstraint)

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



