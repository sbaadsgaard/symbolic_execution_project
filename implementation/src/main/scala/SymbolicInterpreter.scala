import java.util

import Grammar._
import AExp._
import BExp._
import Aop._
import Bop._
import Value._
import Stm._
import scala.collection.mutable
import com.microsoft.z3
import com.microsoft.z3._


class SymbolicInterpreter(symCtx: Context = new Context(new util.HashMap[String, String])) {
  def interpProg(p: Prog,
                 env: mutable.HashMap[Var, SymValue] = mutable.HashMap(),
                 pc: PathConstraint = new PathConstraint(symCtx),
                 symbols: mutable.Set[IntExpr] = mutable.Set()): SymValue = {


    def interpStm(stm: Stm, env: mutable.HashMap[Var, SymValue]): SymValue = {

      stm match {
        case ExpStm(e) => interpAExp(e, env)
        case AssignStm(v, e) =>
          val r = interpAExp(e, env)
          env.update(v, r)
          r
        case CompStm(s1, s2) => interpStm(s1, env); interpStm(s2, env)
        case IfStm(c, thenStm, elseStm) =>
          val cond = interpBExp(c, env)
          cond match {
            case BoolValue(b) => if (b) interpStm(thenStm, env) else interpStm(elseStm, env)
            case SymValue(e: BoolExpr) =>
              val f = symCtx.mkAnd(pc.getFormula, e)
              val fNeg = symCtx.mkAnd(pc.getFormula, symCtx.mkNot(e))
              val s = symCtx.mkSolver()
              s.add(f)
              val thenBranch = s.check()
              s.reset()
              s.add(fNeg)
              val elseBranch = s.check()
              if (thenBranch == Status.SATISFIABLE) {
                if (elseBranch == Status.SATISFIABLE) {
                  interpProg(Prog(p.funcs, elseStm), env, pc.ForkPathConstraint(fNeg), symbols.clone())
                }
                pc.addConstraint(f)
                interpStm(thenStm, env)
              } else if (elseBranch == Status.SATISFIABLE) {
                interpProg(Prog(p.funcs, elseStm), env, pc.ForkPathConstraint(fNeg), symbols.clone())
              } else {
                s.reset()
                s.add(pc.getFormula)
                val model = s.getModel
                val msg = "Current state is infeasible\n " +
                  s"status of then branch: $thenBranch \n" +
                  s"status of else branch: $elseBranch \n" +
                  s"path constratint: $pc \n" +
                  s"model satisfying pc: $model"
                throw new IllegalStateException(msg)
              }
          }
        case _ => throw new UnsupportedOperationException("method not implemented")
      }
    }

    def interpAExp(e: AExp, env: mutable.HashMap[Var, SymValue]): SymValue = {

      e match {
        case Integer(i) => SymValue(symCtx.mkInt(i));
        case Sym(s) =>
          val const = symCtx.mkIntConst(s)
          symbols.add(const)
          SymValue(const)
        case v: Var =>
          val opt = env.get(v)
          opt match {
            case None => throw new NoSuchElementException(s"variable ${v.name} not defined")
            case Some(value) => value
          }
        case bin: AExp.BinExp =>
          val v1 = interpAExp(bin.e1, env).e.asInstanceOf[ArithExpr]
          val v2 = interpAExp(bin.e2, env).e.asInstanceOf[ArithExpr]
          bin.op match {
            case Plus() => SymValue(symCtx.mkAdd(v1, v2))
            case Sub() => SymValue(symCtx.mkSub(v1, v2))
            case Mul() => SymValue(symCtx.mkMul(v1, v2))
            case Mul() => SymValue(symCtx.mkDiv(v1, v2))
          }
        case CallExp(name, args) => {
          val d = p.funcs.get(name)
          d match {
            case None => throw new NoSuchElementException(s"function $name  not defined")
            case Some(f) =>
              val vals = args.map((exp: AExp) => interpAExp(exp, env))
              val argValPairs = f.args zip vals
              val addTo = (m: mutable.HashMap[Var, SymValue], t: (Var, SymValue)) => {
                m.update(t._1, t._2)
                m
              }
              val venvCopy = env.clone()
              val newEnv = argValPairs.foldLeft(venvCopy)(addTo)
              interpStm(f.fbody, newEnv)
          }
        }
        case _ => throw new UnsupportedOperationException("method not implemented")
      }
    }


    def interpBExp(e: BExp, env: mutable.HashMap[Var, SymValue]): Value = {
      e match {
        case Bool(b) => BoolValue(b)
        case BExp.BinExp(e1, e2, op) =>
          val v1 = interpAExp(e1, env).e.asInstanceOf[ArithExpr]
          val v2 = interpAExp(e2, env).e.asInstanceOf[ArithExpr]
          op match {
            case GtOp() => SymValue(symCtx.mkGt(v1, v2))
            case EqOp() => SymValue(symCtx.mkEq(v1, v2))
          }
      }
    }

    val res = interpStm(p.stm, env)
    val resSimpl = res.e.simplify()
    val s = symCtx.mkSolver()
    s.add(pc.getFormula)
    val model = {
      s.check(); s.getModel
    }
    val msg = "******************************* \n" +
      s"execution returned: $resSimpl \n" +
      s"path constraint is: ${pc.getFormula} \n" +
      s"model satisfying pc is: ${modelToString(model, symbols)} \n" +
      "******************************* \n"
    println(msg)
    res
  }

  def modelToString(m: Model, symbols: mutable.Set[IntExpr]): String = {
    val symValPairs = symbols.map((sym: IntExpr) => (sym.getSExpr, m.eval(sym, false)))
    symValPairs.toString()
  }

  def testZ3() = {
    val ctx = new z3.Context(new util.HashMap[String, String])
    val a = ctx.mkBoolConst("a")
    val b = ctx.mkBoolConst("b")
    val c = ctx.mkBoolConst("c")

    val f = ctx.mkAnd(ctx.mkOr(a, b), c)
    val s = ctx.mkSolver()
    val m = 4
    s.add(f)
    println(s.check())
  }

}
