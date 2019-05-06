package grammars

import grammars.ConcreteGrammar.Exp.{CallExp, Var}

import scala.collection.immutable.HashMap

object ConcreteGrammar {


  sealed trait ConcreteValue

  object ConcreteValue {

    case class True() extends ConcreteValue

    case class False() extends ConcreteValue

    case class IntValue(v: Int) extends ConcreteValue

    case class UnitValue() extends ConcreteValue

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

    case class Lt() extends BOp

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

    case class CallExp(id: Id, args: List[Exp]) extends Exp

  }

  sealed trait Stm

  object Stm {

    case class AssignStm(v: Var, e: Exp) extends Stm

    case class IfStm(cond: Exp, thenStm: Stm, elsStm: Stm) extends Stm

    case class WhileStm(cond: Exp, doStm: Stm) extends Stm

    case class AssertStm(cond: Exp) extends Stm

    case class SeqStm(s1: Stm, s2: Stm) extends Stm

    case class ExpStm(e: Exp) extends Stm

  }


  case class FDecl(name: Id, params: List[Id], stm: Stm)


  case class Prog(funcs: HashMap[Id, FDecl], fCall: CallExp)

}
