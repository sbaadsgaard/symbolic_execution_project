package grammars

import grammars.ConcreteGrammar.Result.{Error, Ok}

import scala.collection.immutable.HashMap

object ConcreteGrammar {

  sealed trait Result {
    // These methods are inspired by Chapter 4 of Functional programming in scala. More specific, the Either trait.

    def map(f: ConcreteValue => ConcreteValue): Result = this match {
      case Ok(v) => Ok(f(v))
      case Error(msg) => Error(msg)
    }

    def flatMap(f: ConcreteValue => Result): Result = this match {
      case Ok(v) => f(v)
      case Error(msg) => Error(msg)
    }

    def map2(b: Result)(f: (ConcreteValue, ConcreteValue) => ConcreteValue): Result =
      for {
        a <- this; b1 <- b
      } yield f(a, b1)

    //inspired by filter for Option. We supply a default instance of Error, which will be returned if the condition fails
    def filterWithDefault(default: Result)(f: ConcreteValue => Boolean): Result = this match {
      case Error(_) => this
      case Ok(v) => if (f(v)) this else default
    }
  }

  object Result {

    case class Ok(v: ConcreteValue) extends Result

    case class Error(msg: String) extends Result

    def Try(a: => ConcreteValue): Result =
      try Ok(a)
      catch {
        case e: Exception => Error(e.getLocalizedMessage)
      }
  }

  sealed trait ConcreteValue

  object ConcreteValue {

    case class IntValue(v: Int) extends ConcreteValue

    case class UnitValue() extends ConcreteValue

  }

  sealed trait Input

  object Input {
    case class Concrete(e: Exp) extends Input
  }

  sealed trait AOp

  object AOp {

    case class Add() extends AOp

    case class Sub() extends AOp

    case class Mul() extends AOp

    case class Div() extends AOp

  }

  sealed trait BOp

  object BOp {

    case class Gt() extends BOp

    case class Eq() extends BOp

  }

  case class Id(s: String)

  sealed trait Exp

  object Exp {

    case class Lit(v: ConcreteValue) extends Exp

    case class Var(id: Id) extends Exp

    case class AExp(e1: Exp, e2: Exp, op: AOp) extends Exp

    case class BExp(e1: Exp, e2: Exp, op: BOp) extends Exp

    case class AssignExp(v: Var, e: Exp) extends Exp

    case class IfExp(c: Exp, thenExp: Exp, elseExp: Exp) extends Exp

    case class WhileExp(c: Exp, doExp: Exp) extends Exp

    case class CallExp(id: Id, args: List[Input.Concrete]) extends Exp

    case class ComExp(e1: Exp, e2: Exp) extends Exp

  }


  case class FDecl(name: Id, params: List[Id], body: Exp)

  case class Prog(funcs: HashMap[String, FDecl], e: Exp)

}
