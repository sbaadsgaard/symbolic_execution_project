package interpreters

import grammars.ConcreteGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.ConcreteGrammar.BOp._
import grammars.ConcreteGrammar.ConcreteValue.{False, IntValue, True, UnitValue}
import grammars.ConcreteGrammar.Exp._
import grammars.ConcreteGrammar._
import grammars.SymbolicGrammar.SymbolicValue
import result.{Error, Ok, Result}

import scala.collection.immutable.HashMap
import scala.collection.mutable

class ConcreteInterpreter {
  def interpExp(p: Prog, e: Exp, env: HashMap[Id, ConcreteValue]): Result[(ConcreteValue, HashMap[Id, ConcreteValue]), String] =
    e match {
      case Lit(v) => Ok((v, env))
      case Var(id) => env.get(id) match {
        case None => Error(s"variable ${id.s} not defined")
        case Some(value) => Ok((value, env))
      }

      case AssignExp(v, exp) => handleAssignment(exp, v.id, env, p)

      case AExp(e1, e2, op) =>
        for {
          v1 <- interpExp(p, e1, env)
          v2 <- interpExp(p, e2, v1._2)
          r <- (v1._1, v2._1) match {
            case (i: IntValue, j: IntValue) => op match {
              case Add() => Ok(IntValue(i.v + j.v), v2._2)
              case Sub() => Ok(IntValue(i.v - j.v), v2._2)
              case Mul() => Ok(IntValue(i.v * j.v), v2._2)
              case Div() if j.v == 0 => Error("division by zero")
              case Div() => Ok(IntValue(i.v / j.v), v2._2)
            }
            case _ => Error("arithmetic operations on non integer values")
          }
        } yield r

      case BExp(e1, e2, op) =>
        for {
          v1 <- interpExp(p, e1, env)
          v2 <- interpExp(p, e2, v1._2)
          r <- (v1._1, v2._1) match {
            case (i: IntValue, j: IntValue) => op match {
              case Leq() => if (i.v <= j.v) Ok(True(), v2._2) else Ok(False(), v2._2)
              case Geq() => if (i.v >= j.v) Ok(True(), v2._2) else Ok(False(), v2._2)
              case Lt() => if (i.v < j.v) Ok(True(), v2._2) else Ok(False(), v2._2)
              case Gt() => if (i.v > j.v) Ok(True(), v2._2) else Ok(False(), v2._2)
              case Eq() => if (i.v == j.v) Ok(True(), v2._2) else Ok(False(), v2._2)
            }
            case _ => Error("comparsion of non integer values")
          }
        } yield r


      case IfExp(c, thenExp, elseExp) =>
        interpExp(p, c, env).flatMap({
          case (True(), env1) => interpExp(p, thenExp, env1)
          case (False(), env1) => interpExp(p, elseExp, env1)
          case _ => Error("non boolean value as condition in if-expression")
        })

      case wexp: WhileExp => interpExp(p, wexp.cond, env).flatMap({
        case (True(), env1) => interpExp(p, wexp.doExp, env1).flatMap(res => interpExp(p, wexp, res._2))
        case (False(), env1) => Ok(UnitValue(), env1)
        case _ => Error("non boolean value as condition in while-expression")
      })
      /*
      case CallExp(id, args) => p.funcs.get(id.s) match {
        case None => Error(s"Undefined function $id")
        case Some(f) if args.length != f.params.length => Error("formal and actual parameter list differ in length")
        case Some(f) =>
          for {
            localEnv <- Result.foldLeft(args.zip(f.params))(Ok(env))((env1, t) => env1.flatMap(handleAssignment(t._1, t._2, _, p)).map(_._2))
            res <- interpExp(p, f.body, localEnv)
          } yield (res._1, env)
      }

       */
      case SeqExp(e1, e2) =>
        for {
          v1 <- interpExp(p, e1, env)
          v2 <- interpExp(p, e2, v1._2)
        } yield v2
    }

  def handleAssignment(exp: Exp, id: Id, env: HashMap[Id, ConcreteValue], p: Prog): Result[(ConcreteValue, HashMap[Id, ConcreteValue]), String] =
    interpExp(p, exp, env).flatMap({
      case (UnitValue(), _) => Error(s"Assignment of unit value to variable ${id.s}")
      case v => Ok(v._1, v._2 + (id -> v._1))
    })

}
