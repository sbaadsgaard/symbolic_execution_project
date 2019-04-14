package interpreters

import grammars.ConcreteGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.ConcreteGrammar.Bop.{Eq, Gt}
import grammars.ConcreteGrammar.ConcreteValue.{BoolValue, IntValue, UnitValue}
import grammars.ConcreteGrammar.Exp.{AExp, AssignExp, BExp, CallExp, ComExp, IfExp, Lit, Var, WhileExp}
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
        val error = Error("Arithmetic operation on non integer values")
        interpExp(e1, env).filterWithDefault(error)(_.isInstanceOf[IntValue])
          .map2(interpExp(e2, env).filterWithDefault(error)(_.isInstanceOf[IntValue])
            .filterWithDefault(Error("division by zero"))(_.asInstanceOf[IntValue].v != 0))((v1, v2) => {
            val i = v1.asInstanceOf[IntValue].v
            val j = v2.asInstanceOf[IntValue].v
            op match {
              case Add() => IntValue(i + j)
              case Sub() => IntValue(i - j)
              case Mul() => IntValue(i * j)
              case Div() => IntValue(i / j)
            }
          })
      case BExp(e1, e2, op) =>
        val error = Error("Comparison of non integer values")
        interpExp(e1, env).filterWithDefault(error)(_.isInstanceOf[IntValue])
          .map2(interpExp(e2, env).filterWithDefault(error)(_.isInstanceOf[IntValue]))((v1, v2) => op match {
            case Gt() => BoolValue(v1.asInstanceOf[IntValue].v > v2.asInstanceOf[IntValue].v)
            case Eq() => BoolValue(v2.asInstanceOf[IntValue].v == v2.asInstanceOf[IntValue].v)
          })
      case AssignExp(v, exp) =>
        handleAssignment(exp, v.id, env)

      case IfExp(c, thenExp, elseExp) =>
        interpExp(c, env)
          .flatMap(v => if (v.asInstanceOf[BoolValue].b) interpExp(thenExp, env) else interpExp(elseExp, env))

      case WhileExp(c, doExp) =>
        def go(): Result =
          interpExp(c, env)
            .flatMap(v => if (v.asInstanceOf[BoolValue].b) {
              interpExp(doExp, env)
              go()
            } else Ok(UnitValue()))

        go()

      case CallExp(id, args) => p.funcs.get(id) match {
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
      case ComExp(e1, e2) => interpExp(e1, env)
        .map2(interpExp(e2, env))((_, r2) => r2)
    }

    def handleAssignment(exp: Exp, id: Id, env: mutable.HashMap[Id, ConcreteValue]): Result =
      interpExp(exp, env)
        .filterWithDefault(Error("assigning non integer to variable"))(_.isInstanceOf[IntValue])
        .flatMap(r => {
          env.update(id, r)
          Ok(r)
        })

    interpExp(p.e, env)
  }
}
