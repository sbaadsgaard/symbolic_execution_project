package grammars

import grammars.SymbolicGrammar.SymbolicValue.{SymbolicBool, SymbolicInt}

import scala.collection.immutable.HashMap

object SymbolicGrammar {

  sealed trait SymbolicValue

  object SymbolicValue {

    sealed trait SymbolicInt extends SymbolicValue

    sealed trait SymbolicBool extends SymbolicValue

    case class UnitValue() extends SymbolicValue

  }

  object SymbolicInt {
    case class IntValue(v: Int) extends SymbolicInt
    case class Symbol(s: String) extends SymbolicInt
    case class SymbolicAExp(s1: SymbolicInt, s2: SymbolicInt, op: AOp) extends SymbolicInt
  }

  object SymbolicBool {
    case class True() extends SymbolicBool
    case class False() extends SymbolicBool
    case class SymbolicBExp(i: SymbolicInt, j: SymbolicInt, op: BOp) extends SymbolicBool
    case class Not(b: SymbolicBool) extends SymbolicBool
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

    case class Leq() extends BOp
    case class Geq() extends BOp
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

    case class IfExp(c: Exp, thenExp: Exp, elseExp: Exp) extends Exp

    case class WhileExp(c: Exp, doExp: Exp) extends Exp

    case class CallExp(id: Id, args: List[Lit]) extends Exp

    case class SeqExp(e1: Exp, e2: Exp) extends Exp

  }

  case class Prog(funcs: HashMap[String, FDecl], e: Exp)

  case class FDecl(name: Id, params: List[Id], body: Exp)

}
