import Grammar._
import AExp._
import BExp._
import Aop._
import Bop._
import Value._
import Stm._
import scala.collection.mutable


class SymbolicInterpreter {
  def interpProg(p: Prog): SymValue = {

    def interpStm(stm: Stm, env: mutable.HashMap[Var, SymValue]): SymValue = {

      stm match {
        case ExpStm(e) => interpAExp(e, env)
        case AssignStm(v, e) =>
          val r = interpAExp(e, env)
          env.update(v, r)
          r
        case CompStm(s1, s2) => interpStm(s1, env); interpStm(s2, env)
        case _ => throw new UnsupportedOperationException("method not implemented")
      }
    }

    def interpAExp(e: AExp, env: mutable.HashMap[Var, SymValue]): SymValue = {

      e match {
        case sym: Sym => SymValue(sym)
        case i: Integer => SymValue(i)
        case v: Var =>
          val r = env.get(v)
          r match {
            case None => throw new NoSuchElementException(s"variable ${v.name} is not defined")
            case Some(value) => value
          }

        case AExp.BinExp(e1, e2, op) =>
          val v1 = interpAExp(e1, env)
          val v2 = interpAExp(e2, env)
          SymValue(AExp.BinExp(v1.e, v2.e, op))
        case CallExp(name, args) =>
          val d = p.funcs.get(name)
          d match {
            case None => throw new NoSuchElementException(s"function $name not defined")
            case Some(f) =>
              val vals = args.map((exp: AExp) => interpAExp(exp, env))
              val argValPairs = f.args zip vals
              val addTo = (m: mutable.HashMap[Var, SymValue], t: (Var, SymValue)) => {
                m.update(t._1, t._2)
                m
              }
              val envCopy = env.clone()
              argValPairs.foldLeft(envCopy)(addTo)
              interpStm(f.fbody, envCopy)
          }
        case _ => throw new UnsupportedOperationException("method not implemented")
      }
    }

    def interpBExp(e: BExp, env: mutable.HashMap[Var, SymValue]): SymValue = {
      throw new UnsupportedOperationException("method not implemented")
    }



    val env:mutable.HashMap[Var, SymValue] = mutable.HashMap()
    interpStm(p.stm, env)
  }

  def symExprToString(e: AExp): String = {
    e match {
      case Sym(s) => s
      case Integer(i) => i.toString
      case AExp.BinExp(e1, e2, op) =>
        op match {
          case Plus() => symExprToString(e1) + " + " + symExprToString(e2)
          case Sub() => symExprToString(e1) + " - " + symExprToString(e2)
          case Mul() => symExprToString(e1) + " * " + symExprToString(e2)
          case Div() => "(" + symExprToString(e1) + ") + (" + symExprToString(e2) + ")"
        }
    }
  }

}
