package grammars

import com.microsoft.z3._
import grammars.SymbolicGrammar.Result.{Error, Ok}

import scala.collection.immutable.HashMap

object SymbolicGrammar {

  sealed trait Result {
    //inspired by filter for Option. We supply a default instance of Error, which will be returned if the condition fails
    def filterWithDefault(default: Result)(f: SymbolicValue => Boolean): Result = this match {
      case Error(msg) => Error(msg)
      case Ok(v) => if (f(v)) this else default
    }

    def map(f: SymbolicValue => SymbolicValue): Result = this match {
      case Error(msg) => this
      case Ok(v) => Ok(f(v))
    }

    def flatMap(f: SymbolicValue => Result): Result = this match {
      case Error(_) => this
      case Ok(v) => f(v)
    }

    def map2(b: Result)(f: (SymbolicValue, SymbolicValue) => SymbolicValue) =
      for {
        a <- this; b1 <- b
      } yield f(a, b1)
  }

  object Result {

    case class Ok(v: SymbolicValue) extends Result

    case class Error(msg: String) extends Result

  }

  sealed trait Input

  object Input {
    case class Concrete(e: Exp)
    case class Symbolic(s: String)
  }

  sealed trait SymbolicValue

  object SymbolicValue {

    case class Constraint(e: ArithExpr) extends  SymbolicValue

    case class IntValue(v: Int) extends SymbolicValue

    case class BoolValue(b: Boolean) extends SymbolicValue

    case class UnitValue() extends SymbolicValue

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

    case class Lit(v: SymbolicValue) extends Exp

    case class Var(id: Id) extends Exp

    case class AExp(e1: Exp, e2: Exp, op: AOp) extends Exp

    case class BExp(e1: Exp, e2: Exp, op: BOp) extends Exp

    case class AssignExp(v: Var, e: Exp) extends Exp

    case class IfExp(c: BExp, thenExp: Exp, elseExp: Exp) extends Exp

    case class WhileExp(c: BExp, doExp: Exp) extends Exp

    case class CallExp(id: String, args: List[Input]) extends Exp

    case class ComExp(e1: Exp, e2: Exp) extends Exp

  }

  case class Prog(funcs: HashMap[String, FDecl], e: Exp)


  case class FDecl(name: String, params: List[Id], body: Exp)

}
