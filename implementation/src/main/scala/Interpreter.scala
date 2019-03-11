import java.util.NoSuchElementException
import Grammar._
import AExp._
import BExp._
import Aop._
import Bop._
import ConcreteValue._
import Stm._
import scala.collection.mutable

/**
  * Simple interpreter for the language SImPL. We simply descent the AST.
  */
class Interpreter {

  /** ***********************************************************************/
  /**
    * We define the language here
    */



  def interpProg(p: Prog): ConcreteValue = {

    /*
      We interpret a statement in the current environment @venv and return a possibly updated environment
     */
    def interpStm(statement: Stm, venv: mutable.HashMap[Var, IntValue]): ConcreteValue = {
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

        case AssertStm(c) =>
          val assert = interpBexp(c, venv)
          if (assert.b) Unit() else {println(s"Assertion violation: ${c}"); ErrorValue()}
      }
    }

    def interpWhile(cond: BExp, stm: Stm, venv: mutable.HashMap[Var, IntValue]): ConcreteValue = {
      def dowork(): ConcreteValue = {
        val v = interpBexp(cond, venv)
        if (v.b) {interpStm(stm, venv); dowork()} else Unit()
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
        case Sym(s) => throw new UnsupportedOperationException("Symbolic values not supported in this interpreter")
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
}




