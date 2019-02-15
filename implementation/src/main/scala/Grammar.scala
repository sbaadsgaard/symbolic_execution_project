import scala.collection.mutable

/**
  * Grammar that defines SImPL
  */

object Grammar {

  sealed trait Value

  object Value {

    case class IntValue(x: Int) extends Value

    case class BoolValue(b: Boolean) extends Value

    case class Unit() extends Value

  }

  sealed trait BExp

  object BExp {

    case class Bool(b: Boolean) extends BExp

    case class BinExp(e1: AExp, e2: AExp, op: Bop) extends BExp

  }

  sealed trait Bop

  object Bop {

    case class GtOp() extends Bop

    case class EqOp() extends Bop

  }

  sealed trait AExp

  object AExp {

    case class Integer(i: Int) extends AExp

    case class Var(name: String) extends AExp

    case class BinExp(e1: AExp, e2: AExp, op: Aop) extends AExp

    case class CallExp(name: String, args: List[AExp]) extends AExp

  }

  sealed trait Aop

  object Aop {

    case class Plus() extends Aop

    case class Sub() extends Aop

    case class Mul() extends Aop

    case class Div() extends Aop

  }

  sealed trait Stm

  object Stm {

    case class ExpStm(e: AExp) extends Stm

    case class AssignStm(v: AExp.Var, e: AExp) extends Stm

    case class CompStm(s1: Stm, s2: Stm) extends Stm

    case class IfStm(c: BExp, thenStm: Stm, elseSTm: Stm) extends Stm

    case class WhileStm(c: BExp, doStm: Stm) extends Stm

  }

  case class FDecl(name: String, args: List[AExp.Var], fbody: Stm)


  /*
   * A program consist of zero or more top level function declarations, followed by 1 or more statements starting with
   * @stm
   */
  case class Prog(funcs: mutable.HashMap[String, FDecl], stm: Stm)


}
