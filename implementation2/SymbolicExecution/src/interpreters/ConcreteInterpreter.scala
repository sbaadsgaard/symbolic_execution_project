package interpreters

import grammars.ConcreteGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.ConcreteGrammar.BOp._
import grammars.ConcreteGrammar.ConcreteValue.{False, IntValue, True, UnitValue}
import grammars.ConcreteGrammar.Exp._
import grammars.ConcreteGrammar._
import result.{Error, Ok, Result}

import scala.collection.mutable

class ConcreteInterpreter {
  def interpExp(p: Prog, e: Exp, env: mutable.HashMap[Id, ConcreteValue]): Result[ConcreteValue, String] = {
    e match {

      case Lit(v) => Ok(v)
      case Var(id) => env.get(id) match {
        case None => Error(s"variable ${id.s} not defined")
        case Some(value) => Ok(value)
      }

      case AssignExp(v, exp) => handleAssignment(exp, v.id, env)

      case AExp(e1, e2, op) =>
        for {
          v1 <- interpExp(p, e1, env)
          v2 <- interpExp(p, e2, env)
          r <- (v1, v2) match {
            case (i: IntValue, j: IntValue) => op match {
              case Add() => Ok(IntValue(i.v + j.v ))
              case Sub() => Ok(IntValue(i.v - j.v))
              case Mul() => Ok(IntValue(i.v * j.v))
              case Div() => if (j.v == 0) Error("division by zero") else Ok(IntValue(i.v / j.v))
            }
            case _ => Error("arithmetic operation on non integer values")
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


      case AssignExp(v, exp) => handleAssignment(exp, v.id, env)

      case IfExp(c, thenExp, elseExp) =>
        interpExp(p, c, env).flatMap({
          case True() => interpExp(p, thenExp, env)
          case False() => interpExp(p, elseExp, env)
        })

      case WhileExp(c, doExp) =>
        def go(): Result[ConcreteValue, String] = interpExp(p, c, env).flatMap({
          case True() =>
            interpExp(p, doExp, env)
            go()
          case False() =>
            Ok(UnitValue())
          case _ => Error("non boolean condition in while expression")
        })

        go()

      case CallExp(id, args) => p.funcs.get(id.s) match {
        case None => Error(s"Undefined function $id")
        case Some(f) =>
          if (args.length == f.params.length) {
            val localEnv = env.clone()
            args.zip(f.params).map(p => handleAssignment(p._1, p._2, localEnv)).find(_.isInstanceOf[Error]) match {
              case Some(err) => err
              case None => interpExp(p, f.body, localEnv)
            }
          } else Error("formal and actual parameter list differ in length")
      }
      case SeqExp(e1, e2) =>
        for {
          _ <- interpExp(p, e1, env)
          v <- interpExp(p, e2, env)
        } yield v
    }

    def handleAssignment(exp: Exp, id: Id, env: mutable.HashMap[Id, ConcreteValue]): Result[ConcreteValue, String] =
      interpExp(p, exp, env).flatMap({
        case UnitValue() => Error(s"assignment of unit value to variable $id")
        case k =>
          env.update(id, k)
          Ok(k)
      })
  }
}
