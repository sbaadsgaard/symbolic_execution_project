import java.util.NoSuchElementException

import Grammar._
import AExp._
import BExp._
import Aop._
import Bop._
import Value._
import Stm._


import scala.collection.mutable

/**
  * Simple interpreter for the language SImPL. We simply descent the AST.
  */
object Interpreter extends App {

  /** ***********************************************************************/
  /**
    * We define the language here
    */



  def interpProg(p: Prog): Value = {

    /*
      We interpret a statement in the current environment @venv and return a possibly updated environment
     */
    def interpStm(statement: Stm, venv: mutable.HashMap[Var, IntValue]): Value = {
      statement match {
        case ExpStm(e) => interpAexp(e, venv)
        case AssignStm(id, valExp) =>
          val v = interpAexp(valExp, venv)
          venv.update(id, v)
          v
        case CompStm(s1, s2) =>
          interpStm(s1, venv)
          interpStm(s2, venv)
        case IfStm(cond, thenStm, elseStm) =>
          val v = interpBexp(cond, venv)
          if (v.b) interpStm(thenStm, venv) else interpStm(elseStm, venv)

        case WhileStm(cond, stm) =>
          interpWhile(cond, stm, venv)

      }
    }

    def interpWhile(cond: BExp, stm: Stm, venv: mutable.HashMap[Var, IntValue]): Value = {
      def dowork(): Value = {
        val v = interpBexp(cond, venv)
        if (v.b) interpStm(stm, venv) else Unit()
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
            case i: IntValue => i
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
        case Integer(i) => IntValue(i)
        case v: Var =>
          val res = venv.get(v)
          res match {
            case None => throw new NoSuchElementException(s"variable ${v.name} not defined")
            case Some(r) => r
          }
        case bin: AExp.BinExp => interpBinAExp(bin, venv)
        case cExp: CallExp => interpCall(cExp, venv)
      }
    }

    def interpBinBExp(e: BExp.BinExp, venv: mutable.HashMap[Var, IntValue]): BoolValue = {
      val v1 = interpAexp(e.e1, venv)
      val v2 = interpAexp(e.e2, venv)
      e.op match {
        case GtOp() => BoolValue(v1.x > v2.x)
        case EqOp() => BoolValue(v1.x == v2.x)
      }
    }

    def interpBinAExp(e: AExp.BinExp, venv: mutable.HashMap[Var, IntValue]): IntValue = {
      val v1 = interpAexp(e.e1, venv)
      val v2 = interpAexp(e.e2, venv)
      e.op match {
        case Plus() => IntValue(v1.x + v2.x)
        case Sub() => IntValue(v1.x - v2.x)
        case Mul() => IntValue(v1.x * v2.x)
        case Div() => IntValue(v1.x / v2.x)
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
    IfStm(
      BExp.BinExp(Integer(2), Var("n"), GtOp()),
      ExpStm(Var("n")),
      ExpStm(
        BinExp(
          CallExp("fib", List(BinExp(Var("n"), Integer(1), Sub()))),
          CallExp("fib", List(BinExp(Var("n"), Integer(2), Sub()))),
          Plus()
        )
      )))),
    ExpStm(CallExp("fib", List(Integer(12)))))


  /*
   * Test to ensure that locally defined variables does not escape function scope
   */

  val testProg1 = Prog(mutable.HashMap("test" -> FDecl("test", List(Var("a")),
    ExpStm(BinExp(Var("a"), Integer(2), Mul())))),
    CompStm(
      ExpStm(CallExp("test", List(Integer(2)))),
      ExpStm(Var("a"))
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




