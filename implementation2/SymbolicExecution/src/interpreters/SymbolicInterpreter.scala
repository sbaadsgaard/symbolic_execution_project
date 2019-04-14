package interpreters

import com.microsoft.z3._
import grammars.SymbolicGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.SymbolicGrammar.Exp.{AExp, Lit, Var}
import grammars.SymbolicGrammar.Result._
import grammars.SymbolicGrammar.SymbolicValue.{Constraint, IntValue}
import grammars.SymbolicGrammar._

import scala.collection.mutable

class SymbolicInterpreter {

  def interpProg(p: Prog, env: mutable.HashMap[Id, SymbolicValue], ctx: Context): List[Result] = {
    def interpExp(e: Exp, env: mutable.HashMap[Id, SymbolicValue]): Result = e match {
      case Lit(v) => v match {
          // we map integer literals to integer constants in Z3 to simplify evaluation of arithmetic expressions. 
        case IntValue(value) => Ok(Constraint(ctx.mkInt(value)))
        case _ => Ok(v)
      }
      case Var(id) => env.get(id) match {
        case None => Error(s"variable ${id.s} not defined")
        case Some(value) => Ok(value)
      }
      case AExp(e1, e2, op) =>
        val error = Error("arithmetic operations on non integer values")
        interpExp(e1, env).filterWithDefault(error)(_.isInstanceOf[Constraint])
          .map2(interpExp(e2, env).filterWithDefault(error)(_.isInstanceOf[Constraint]))(handleAExp(_, _, op))

      case _ => throw new Exception("not implemented")
    }

    def handleAExp(s1: SymbolicValue, s2: SymbolicValue, op: AOp): SymbolicValue = {
      val v1 = s1.asInstanceOf[Constraint].e
      val v2 = s2.asInstanceOf[Constraint].e
      op match {
        case Add() => Constraint(ctx.mkAdd(v1, v2))
        case Sub() => Constraint(ctx.mkSub(v1, v2))
        case Mul() => Constraint(ctx.mkMul(v1, v2))
        case Div() => Constraint(ctx.mkDiv(v1, v2))
      }
    }
    List(interpExp(p.e, env).map {
      case Constraint(e) => Constraint(e.simplify().asInstanceOf[ArithExpr])
      case v => v
    })
  }
}
