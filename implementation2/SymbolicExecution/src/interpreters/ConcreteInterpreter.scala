package interpreters

import grammars.ConcreteGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.ConcreteGrammar.BOp._
import grammars.ConcreteGrammar.ConcreteValue.{False, IntValue, True, UnitValue}
import grammars.ConcreteGrammar.Exp._
import grammars.ConcreteGrammar.Stm.{AssertStm, AssignStm, ExpStm, IfStm, SeqStm, WhileStm}
import grammars.ConcreteGrammar._
import result.{Error, Ok, Result}

import scala.collection.immutable.HashMap

class ConcreteInterpreter {
  def interpExp(p: Prog, e: Exp, env: HashMap[Id, ConcreteValue]): Result[ConcreteValue, String] =
    e match {
      case Lit(v) => Ok(v)
      case Var(id) => env.get(id) match {
        case None => Error(s"variable ${id.s} not defined")
        case Some(value) => Ok(value)
      }

      case AExp(e1, e2, op) =>
        for {
          v1 <- interpExp(p, e1, env)
          v2 <- interpExp(p, e2, env)
          r <- (v1, v2) match {
            case (i: IntValue, j: IntValue) => op match {
              case Add() => Ok(IntValue(i.v + j.v))
              case Sub() => Ok(IntValue(i.v - j.v))
              case Mul() => Ok(IntValue(i.v * j.v))
              case Div() if j.v == 0 => Error("division by zero")
              case Div() => Ok(IntValue(i.v / j.v))
            }
            case _ => Error("arithmetic operations on non integer values")
          }
        } yield r

      case BExp(e1, e2, op) =>
        for {
          v1 <- interpExp(p, e1, env)
          v2 <- interpExp(p, e2, env)
          r <- (v1, v2) match {
            case (i: IntValue, j: IntValue) => op match {
              case Leq() => if (i.v <= j.v) Ok(True()) else Ok(False())
              case Geq() => if (i.v >= j.v) Ok(True()) else Ok(False())
              case Lt() => if (i.v < j.v) Ok(True()) else Ok(False())
              case Gt() => if (i.v > j.v) Ok(True()) else Ok(False())
              case Eq() => if (i.v == j.v) Ok(True()) else Ok(False())
            }
            case _ => Error("comparsion of non integer values")
          }
        } yield r

      case CallExp(id, args) => p.funcs.get(id.s) match {
        case None => Error(s"Undefined function $id")
        case Some(f) if args.length != f.params.length => Error("formal and actual parameter list differ in length")
        case Some(f) =>
          Result.traverse(args)(interpExp(p, _, env)).flatMap(_.zip(f.params)
            .foldLeft[Result[HashMap[Id, ConcreteValue], String]](Ok(env))((next, t) => next.flatMap(buildEnv(t._1, t._2, _))))
            .flatMap(interpStm(p, f.stm, _).flatMap(r => Ok(r._1)))
      }
    }


  def interpStm(p: Prog, stm: Stm, env: HashMap[Id, ConcreteValue]): Result[(ConcreteValue, HashMap[Id, ConcreteValue]), String] = {
    stm match {
      case AssignStm(v, e) => interpExp(p, e, env).flatMap({
        case UnitValue() => Error(s"Assignment of unit value to variable ${v.id.s}")
        case value => Ok((value, env + (v.id -> value)))
      })
      case IfStm(cond, thenStm, elsStm) => interpExp(p, cond, env).flatMap({
        case True() => interpStm(p, thenStm, env)
        case False() => interpStm(p, elsStm, env)
        case _ => Error("non boolean condition in if expression")
      })
      case wStm: WhileStm => interpExp(p, wStm.cond, env).flatMap({
        case True() => interpStm(p, wStm.doStm, env).flatMap(res => interpStm(p, wStm, res._2))
        case False() => Ok(UnitValue(), env)
        case _ => Error("non boolenan condition in while expression")

      })

      case AssertStm(cond) => interpExp(p, cond, env).flatMap({
        case True() => Ok(UnitValue(), env)
        case False() => Error("Assertion violation")
        case _ => Error("Non boolean condition in assertion statement")
      })

      case SeqStm(s1, s2) =>
        for {
          r1 <- interpStm(p, s1, env)
          r2 <- interpStm(p, s2, r1._2)
        } yield r2

      case ExpStm(e) => interpExp(p, e, env).flatMap(Ok(_, env))
    }
  }


  def buildEnv(v: ConcreteValue, id: Id, env: HashMap[Id, ConcreteValue]): Result[HashMap[Id, ConcreteValue], String] =
    v match {
      case UnitValue() => Error("unit value as argument to function call")
      case value => Ok(env + (id -> value))
    }


}
