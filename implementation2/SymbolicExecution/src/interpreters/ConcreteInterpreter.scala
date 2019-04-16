package interpreters

import grammars.ConcreteGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.ConcreteGrammar.BOp.{Eq, Gt}
import grammars.ConcreteGrammar.ConcreteValue.{IntValue, UnitValue}
import grammars.ConcreteGrammar.Exp._
import grammars.ConcreteGrammar.Result.{Error, Ok}
import grammars.ConcreteGrammar._

import scala.collection.mutable

class ConcreteInterpreter {
  def interpProg(p: Prog, env: mutable.HashMap[Id, ConcreteValue]): Result = {
    def interpExp(e: Exp, env: mutable.HashMap[Id, ConcreteValue]): Result = e match {
      case Lit(v) => Ok(v)
      case Var(id) => env.get(id) match {
        case None => Error(s"Variable ${id.s} not declared")
        case Some(value) => Ok(value)
      }
      case AExp(e1, e2, op) =>
        val error = Error("Arithmetic operations on unit")
        interpExp(e1, env).filterWithDefault(error)(_.isInstanceOf[IntValue])
          .map2(interpExp(e2, env).filterWithDefault(error)(_.isInstanceOf[IntValue])
            .filterWithDefault(Error("division by zero"))(_.asInstanceOf[IntValue].v != 0))(handleAExp(_, _, op))
      case BExp(e1, e2, op) =>
        val error = Error("Comparison of unit")
        interpExp(e1, env).filterWithDefault(error)(_.isInstanceOf[IntValue])
          .map2(interpExp(e2, env).filterWithDefault(error)(_.isInstanceOf[IntValue]))(handleBExp(_, _, op))
      case AssignExp(v, exp) => handleAssignment(exp, v.id, env)

      case IfExp(c, thenExp, elseExp) =>
        interpExp(c, env).filterWithDefault(Error("unit value as condition"))(_.isInstanceOf[IntValue])
          .flatMap(v => if (v.asInstanceOf[IntValue].v != 0) interpExp(thenExp, env) else interpExp(elseExp, env))

      case WhileExp(c, doExp) =>
        def go(): Result =
          interpExp(c, env).filterWithDefault(Error("unit value as condition"))(_.isInstanceOf[IntValue]).
            flatMap(v => if (v.asInstanceOf[IntValue].v != 0) {
              interpExp(doExp, env)
              go()
            } else Ok(UnitValue()))

        go()

      case CallExp(id, args) => p.funcs.get(id.s) match {
        case None => Error(s"Undefined function $id")
        case Some(f) =>
          if (args.length == f.params.length) {
            val localEnv = env.clone()
            args.map(_.e).zip(f.params).map(p => handleAssignment(p._1, p._2, localEnv)).find(_.isInstanceOf[Error]) match {
              case Some(err) => err
              case None => interpExp(f.body, localEnv)
            }
          } else Error("formal and actual parameter list differ in length")
      }
      case ComExp(e1, e2) => interpExp(e1, env).map2(interpExp(e2, env))((_, r2) => r2)
    }

    def handleAssignment(exp: Exp, id: Id, env: mutable.HashMap[Id, ConcreteValue]): Result =
      interpExp(exp, env)
        .filterWithDefault(Error("assigning unit to variable"))(_.isInstanceOf[IntValue])
        .flatMap(r => {
          env.update(id, r)
          Ok(r)
        })

    def handleAExp(v1: ConcreteValue, v2: ConcreteValue, op: AOp): ConcreteValue = {
      val i = v1.asInstanceOf[IntValue].v
      val j = v2.asInstanceOf[IntValue].v
      op match {
        case Add() => IntValue(i + j)
        case Sub() => IntValue(i - j)
        case Mul() => IntValue(i * j)
        case Div() => IntValue(i / j)
      }
    }

    def handleBExp(v1: ConcreteValue, v2: ConcreteValue, op: BOp): ConcreteValue = {
      val i = v1.asInstanceOf[IntValue].v
      val j = v2.asInstanceOf[IntValue].v
      op match {
        case Gt() => IntValue(if (i > j) 1 else 0)
        case Eq() => IntValue(if (i == j) 1 else 0)
      }
    }

    interpExp(p.e, env)
  }
}
