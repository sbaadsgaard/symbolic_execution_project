package interpreters

import grammars.ConcreteGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.ConcreteGrammar.Bop.{Eq, Gt}
import grammars.ConcreteGrammar.ConcreteValue.{BoolValue, IntValue, UnitValue}
import grammars.ConcreteGrammar.Exp.{AExp, AssignExp, BExp, CallExp, ComExp, IfExp, Lit, Var, WhileExp}
import grammars.ConcreteGrammar.Result.{Error, Ok}
import grammars.ConcreteGrammar._

import scala.collection.mutable

class ConcreteInterpreter {
  def interpProg(p: Prog, env: mutable.HashMap[Var, ConcreteValue]): Result = {
    //TODO Try to rewrite interpAExp and InterpBExp. should be able to make it more consise
    def interpExp(e: Exp, env: mutable.HashMap[Var, ConcreteValue]): Result = e match {
      case Lit(v) => Ok(v)
      case v: Var => env.get(v) match {
        case None => Error(s"Variable ${v.id} not declared")
        case Some(value) => Ok(value)
      }
      case AExp(e1, e2, op) =>
        def interpAexp(v1: ConcreteValue, v2: ConcreteValue): IntValue = {
          val i = v1.asInstanceOf[IntValue].v
          val j = v2.asInstanceOf[IntValue].v
          op match {
            case Add() => IntValue(i + j)
            case Sub() => IntValue(i - j)
            case Mul() => IntValue(i * j)
            case Div() => IntValue(i / j)
          }
        }

        val error = Error("Arithmetic operation on non integer values")
        interpExp(e1, env).filterWithDefault(error)(_.isInstanceOf[IntValue])
          .map2(
            interpExp(e2, env).filterWithDefault(error)(_.isInstanceOf[IntValue]))(interpAexp)

      case BExp(e1, e2, op) =>
        def interpBExp(v1: ConcreteValue, v2: ConcreteValue): BoolValue = {
          val i = v1.asInstanceOf[IntValue].v
          val j = v2.asInstanceOf[IntValue].v

          op match {
            case Gt() => BoolValue(i > j)
            case Eq() => BoolValue(i == j)
          }
        }

        val error = Error("Comparison of non integer values")
        interpExp(e1, env).filterWithDefault(error)(_.isInstanceOf[IntValue])
          .map2(interpExp(e2, env).filterWithDefault(error)(_.isInstanceOf[IntValue]))(interpBExp)
      case AssignExp(v, exp) =>
        handleAssignment(exp, v, env)

      case IfExp(c, thenExp, elseExp) => {
        interpExp(c, env)
          .flatMap(v => if (v.asInstanceOf[BoolValue].b) interpExp(thenExp, env) else interpExp(elseExp, env))
      }

      case WhileExp(c, doExp) =>
        def go(): Result =
          interpExp(c, env)
            .flatMap(v => if (v.asInstanceOf[BoolValue].b) {
              interpExp(doExp, env)
              go()
            } else Ok(UnitValue()))

        go()

      case CallExp(id, args) => p.funcs.get(id) match {
        case None => Error(s"undefined function $id")
        case Some(f) => {
          if (args.length == f.params.length) {
            val localEnv = env.clone()
            val res = args.zip(f.params).map(p => handleAssignment(p._1, p._2, localEnv)).find(_.isInstanceOf[Error])
            res match {
              case Some(e) => e
              case None => interpExp(f.body, localEnv)
            }
          } else Error("missing arguments")
        }
      }
      case ComExp(e1, e2) => interpExp(e1, env)
        .map2(interpExp(e2, env))((_, r2) => r2)
    }

    def handleAssignment(exp: Exp, v: Var, env: mutable.HashMap[Var, ConcreteValue]): Result =
      interpExp(exp, env)
        .filterWithDefault(Error("assigning non integer to variable"))(_.isInstanceOf[IntValue])
        .flatMap(r => {
          env.update(v, r)
          Ok(r)
        })

    interpExp(p.e, env)
  }
}
