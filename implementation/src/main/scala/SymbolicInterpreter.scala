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
                 pc: PathConstraint = new PathConstraint(symCtx, symCtx.mkTrue()),
                 symbols: mutable.Set[IntExpr] = mutable.Set(),
                 maxBranches: Int,
                 currBranches: Int): Value = {


    def interpStm(stm: Stm,
                  env: mutable.HashMap[Var, SymValue],
                  next: Option[Stm] = None): Value = {

      stm match {
        case ExpStm(e) => interpAExp(e, env)
        case AssignStm(v, e) =>
          val r = interpAExp(e, env)
          env.update(v, r)
          r
        case CompStm(s1, s2) =>
          interpStm(s1, env, next = Some(s2))
          interpStm(s2, env)
        case IfStm(c, thenStm, elseStm) =>
          val cond = interpBExp(c, env)
          cond match {
            case BoolValue(b) => if (b) interpStm(thenStm, env) else interpStm(elseStm, env)
            case SymValue(e: BoolExpr) =>
              val f = symCtx.mkAnd(pc.formula, e)
              val fNeg = symCtx.mkAnd(pc.formula, symCtx.mkNot(e))
              val s = symCtx.mkSolver()
              val thenBranch = s.check(f)
              val elseBranch = s.check(fNeg)
              if (thenBranch == Status.SATISFIABLE) {
                if (elseBranch == Status.SATISFIABLE) {
                  next match {
                    case None =>
                      interpProg(Prog(p.funcs, elseStm), env.clone(), pc.forkPathConstraint(fNeg), symbols.clone(), maxBranches, currBranches+1)
                    case Some(statement) =>
                      val pr = Prog(p.funcs, CompStm(elseStm, statement))
                      interpProg(pr, env.clone(), pc.forkPathConstraint(fNeg), symbols.clone(), maxBranches, currBranches + 1)
                  }
                }
                pc.addConstraint(f)
                interpStm(thenStm, env, next)
              } else if (elseBranch == Status.SATISFIABLE) {
                next match {
                  case None => interpProg(Prog(p.funcs, elseStm), env.clone(), pc.forkPathConstraint(fNeg), symbols.clone(), maxBranches, currBranches + 1)
                  case Some(statement) =>
                    val pr = Prog(p.funcs, CompStm(elseStm, statement))
                    interpProg(pr, env.clone(), pc.forkPathConstraint(fNeg), symbols.clone(), maxBranches, currBranches + 1)
                }
              } else {
                // TODO figure out what to do with this case. Will it ever happen? and if so, we should handle it more gracefully than throwing an exception
                s.add(pc.formula)
                val model = s.getModel
                val msg = "Current state is infeasible\n " +
                  s"status of then branch: $thenBranch \n" +
                  s"status of else branch: $elseBranch \n" +
                  s"path constratint: $pc \n" +
                  s"model satisfying pc: $model"
                throw new IllegalStateException(msg)
              }
          }

        case ws: WhileStm => interpWhile(ws, env, next)
        case _ => throw new UnsupportedOperationException("method not implemented")
      }
    }

    def interpWhile(ws: Grammar.Stm.WhileStm, env: mutable.HashMap[Var, SymValue], next: Option[Stm]): Value = {
      def dowork(): Value = {
        val cond = interpBExp(ws.c, env)
        cond match {
          case BoolValue(b) => if (b) {
            interpStm(ws.doStm, env)
            dowork()
          } else Unit()
          case SymValue(e: BoolExpr) =>
            val f = symCtx.mkAnd(pc.formula, e)
            val fNeg = symCtx.mkAnd(pc.formula, symCtx.mkNot(e))
            val s = symCtx.mkSolver()
            val continueBranch = s.check(f)
            val endBranch = s.check(fNeg)
            if (endBranch == Status.SATISFIABLE) {
              if (continueBranch == Status.SATISFIABLE) {
                if (currBranches <= maxBranches) {
                  val pr = next match {
                    case None => Prog(p.funcs, CompStm(ws.doStm, ws))
                    case Some(stm) => Prog(p.funcs, CompStm(ws.doStm, CompStm(ws, stm)))
                  }
                  interpProg(pr, env.clone(), pc.forkPathConstraint(f), symbols.clone(), maxBranches, currBranches + 1)
                } else {
                  println("max branches reached - ignoring further loops")
                }
              }
              pc.addConstraint(fNeg)
              next match {
                case None => Unit()
                case Some(stm) => interpStm(stm, env, next)
              }
            } else if (continueBranch == Status.SATISFIABLE) {
              if (currBranches <= maxBranches) {
                val pr = next match {
                  case None => Prog(p.funcs, CompStm(ws.doStm, ws))
                  case Some(stm) => Prog(p.funcs, CompStm(ws.doStm, CompStm(ws, stm)))
                }
                interpProg(pr, env.clone(), pc.forkPathConstraint(f), symbols.clone(), maxBranches, currBranches + 1)
              } else {
                println("max branches reached - ignoring further loops")
                Unit()
              }
            } else {
              s.add(pc.formula)
              val model = s.getModel
              val msg = "Current state is infeasible\n " +
                s"status of continue branch: $continueBranch \n" +
                s"status of end branch: $endBranch \n" +
                s"path constratint: $pc \n" +
                s"model satisfying pc: $model"
              throw new IllegalStateException(msg)
            }
        }
      }
      dowork()
    }

    def interpAExp(e: AExp, env: mutable.HashMap[Var, SymValue]): SymValue = {

      e match {
        case Integer(i) => SymValue(symCtx.mkInt(i));
        case Sym(sym) =>
          val const = symCtx.mkIntConst(sym)
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
            case Div() => SymValue(symCtx.mkDiv(v1, v2))
          }
        case CallExp(name, args) =>
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
              val res = interpStm(f.fbody, newEnv)
              res match {
                case sv: SymValue => sv
                case _ => throw new IllegalArgumentException("functions must return symbolic values")
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
    val s = symCtx.mkSolver()
    s.add(pc.formula)
    val model = {
      s.check()
      s.getModel
    }
    val msg = "******************************* \n" +
      s"execution returned: ${
        res match {
          case SymValue(e) => e.simplify()
          case _ => res
        }
      } \n" +
      s"path constraint is: ${pc.formula} \n" +
      s"model satisfying pc is: ${modelToString(model, symbols)} \n" +
      "******************************* \n"
    println(msg)
    res
  }

  def modelToString(m: Model, symbols: mutable.Set[IntExpr]): String = {
    val symValPairs = symbols.map((sym: IntExpr) => (sym.getSExpr, m.eval(sym, true)))
    symValPairs.toString()
  }

}
