import java.util.NoSuchElementException

import Interpreter.AExp.{CallExp, Integer, Var}
import Interpreter.Aop.{Mul, Plus, Sub}
import Interpreter.BExp.Bool
import Interpreter.Bop.GtOp
import Interpreter.Stm.ExpStm
import Interpreter.Value.{BoolValue, IntValue}

import scala.collection.mutable

/**
  * Simple interpreter for the language SImPL. We simply descent the AST.
  */
object Interpreter extends App {

  /** ***********************************************************************/
  /**
    * We define the language here
    */

  sealed trait Expression

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

    case class AssignStm(v: Var, e: AExp) extends Stm

    case class CompStm(s1: Stm, s2: Stm) extends Stm

    case class IfStm(c: BExp, thenStm: Stm, elseSTm: Stm) extends Stm

    case class WhileStm(c: BExp, doStm: Stm) extends Stm

  }

  case class FDecl(name: String, args: List[Var], fbody: Stm)


  /*
   * A program consist of zero or more top level function declarations, followed by 1 or more statements starting with
   * @stm
   */
  case class Prog(funcs: mutable.HashMap[String, FDecl], stm: Stm)


  def interpProg(p: Prog): Value = {

    /*
      We interpret a statement in the current environment @venv and return a possibly updated environment
     */
    def interpStm(statement: Stm, venv: mutable.HashMap[Var, IntValue]): Value = {
      statement match {
        case Stm.ExpStm(e) => interpAexp(e, venv)
        case Stm.AssignStm(id, valExp) =>
          val v = interpAexp(valExp, venv)
          venv.update(id, v)
          v
        case Stm.CompStm(s1, s2) =>
          interpStm(s1, venv)
          interpStm(s2, venv)
        case Stm.IfStm(cond, thenStm, elseStm) =>
          val v = interpBexp(cond, venv)
          if (v.b) interpStm(thenStm, venv) else interpStm(elseStm, venv)

        case Stm.WhileStm(cond, stm) =>
          interpWhile(cond, stm, venv)

      }
    }

    def interpWhile(cond: BExp, stm: Stm, venv: mutable.HashMap[Var, IntValue]): Value = {
      def dowork(): Value = {
        val v = interpBexp(cond, venv)
        if (v.b) interpStm(stm, venv) else Value.Unit()
      }

      dowork()
    }


    /*
     * We interpret call by first evaluation the expressions given as argument values, and then we add the function
     * arguments to a new local environment, which is passed onto the interpretation of the function body.
     */
    def interpCall(e: CallExp, venv: mutable.HashMap[Var, IntValue]): IntValue = {
      val decl = p.funcs.get(e.name)
      decl match {
        case None => throw new NoSuchElementException(s"function ${e.name} not defined")
        case Some(f) =>
          val vals = e.args.map((exp: AExp) => interpAexp(exp, venv))
          val argValPairs = f.args zip vals
          val addTo = (m: mutable.HashMap[Var, IntValue], t: (Var, IntValue)) => {
            m.update(t._1, t._2)
            m
          }
          val venvCopy = venv.clone()
          val newEnv = argValPairs.foldLeft(venvCopy)(addTo)
          val res = interpStm(f.fbody, newEnv)
          res match {
            case i: Value.IntValue => i
            case _ => throw new IllegalArgumentException("functions can may only return boolean values!")
          }
      }
    }

    def interpBexp(e: BExp, venv: mutable.HashMap[Var, IntValue]): BoolValue = {
      e match {
        case Bool(b) => BoolValue(b)
        case bin: BExp.BinExp => interpBinBExp(bin, venv)
      }
    }

    def interpAexp(e: AExp, venv: mutable.HashMap[Var, IntValue]): IntValue = {
      e match {
        case AExp.Integer(i) => IntValue(i)
        case v: AExp.Var =>
          val res = venv.get(v)
          res match {
            case None => throw new NoSuchElementException(s"variable ${v.name} not defined")
            case Some(r) => r
          }
        case bin: AExp.BinExp => interpBinAExp(bin, venv)
        case cExp: AExp.CallExp => interpCall(cExp, venv)
      }
    }

    def interpBinBExp(e: BExp.BinExp, venv: mutable.HashMap[Var, IntValue]): BoolValue = {
      val v1 = interpAexp(e.e1, venv)
      val v2 = interpAexp(e.e2, venv)
      e.op match {
        case Bop.GtOp() => BoolValue(v1.x > v2.x)
        case Bop.EqOp() => BoolValue(v1.x == v2.x)
      }
    }

    def interpBinAExp(e: AExp.BinExp, venv: mutable.HashMap[Var, IntValue]): IntValue = {
      val v1 = interpAexp(e.e1, venv)
      val v2 = interpAexp(e.e2, venv)
      e.op match {
        case Aop.Plus() => IntValue(v1.x + v2.x)
        case Aop.Sub() => IntValue(v1.x - v2.x)
        case Aop.Mul() => IntValue(v1.x * v2.x)
        case Aop.Div() => IntValue(v1.x / v2.x)
      }
    }

    val venv: mutable.HashMap[Var, IntValue] = mutable.HashMap()
    interpStm(p.stm, venv)
  }

  /*
   * Func to build a Prog from a statement. takes a stm, and returns a prog with a map from id main to func main with
   * the given stm as body
   */

  /*
   * fun fib(n) = if (2 > n) n else fib(n-1) + fib(n-2)
   * fib(12)
   */

  val testProg = Prog(mutable.HashMap("fib" -> FDecl("fib", List(Var("n")),
    Stm.IfStm(
      BExp.BinExp(Integer(2), Var("n"), GtOp()),
      Stm.ExpStm(Var("n")),
      Stm.ExpStm(
        AExp.BinExp(
          CallExp("fib", List(AExp.BinExp(Var("n"), Integer(1), Sub()))),
          CallExp("fib", List(AExp.BinExp(Var("n"), Integer(2), Sub()))),
          Plus()
        )
      )))),
    Stm.ExpStm(CallExp("fib", List(Integer(12)))))


  /*
   * Test to ensure that locally defined variables does not escape function scope
   */

  val testProg1 = Prog(mutable.HashMap("test" -> FDecl("test", List(Var("a")),
    Stm.ExpStm(AExp.BinExp(Var("a"), Integer(2), Mul())))),
    Stm.CompStm(
      Stm.ExpStm(CallExp("test", List(Integer(2)))),
      Stm.ExpStm(Var("a"))
    )
  )

  /*
   * Test to ensure that mutations of global variables only exists in the function scope
   */


  /*
   * fun pow(a, b) {
   *  res = 1
   *  i = 0
   *  while (b > i) do
   *    res = res*a
   *    i = 1 + 1
   *  res
   * }
   *
   * pow(2,3)
   */

  val res = interpProg(testProg1)
  println(s"result was: ${res.toString}")
}




