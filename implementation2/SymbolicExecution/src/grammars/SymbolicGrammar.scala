package grammars

import grammars.SymbolicGrammar.Exp.{CallExp, Var}
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

    case class CallExp(id: Id, args: List[Exp]) extends Exp

  }

  sealed trait Stm

  object Stm {

    case class AssignStm(v: Var, e: Exp) extends Stm

    case class IfStm(cond: Exp, thenStm: Stm, elsStm: Stm) extends Stm

    case class WhileStm(cond: Exp, doStm: Stm) extends Stm

    case class SeqStm(s1: Stm, s2: Stm) extends Stm

    case class Empty() extends Stm
  }
  // a program concists of 1 or more function definitions, followed by a call to one of those functions
  case class Prog(funcs: HashMap[Id, FDecl], fCall: CallExp)

  case class FDecl(name: Id, params: List[Id], body: FBody)

  //a function body consists of zero or more statements, followed by a return expression
  case class FBody(stm: Stm, returnExp: Exp)

}
