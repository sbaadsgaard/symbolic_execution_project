import java.util

import Grammar.AExp._
import Grammar.Aop._
import Grammar.BExp._
import Grammar.Bop._
import Grammar.Stm._
import Grammar.Value._
import Grammar._
import com.microsoft.z3._

import scala.collection.mutable


class SymbolicInterpreter(ctx: Context = new Context(new util.HashMap[String, String])) {
  def interpProg(p: Prog,
                 env: mutable.HashMap[Var, SymValue] = mutable.HashMap(),
                 pc: PathConstraint = new PathConstraint(ctx, ctx.mkTrue()),
                 symbols: mutable.Set[IntExpr] = mutable.Set(),
                 maxBranches: Int,
                 currBranches: Int = 1): Value = {


    def interpStm(stm: Stm,
                  env: mutable.HashMap[Var, SymValue],
                  next: Option[Stm] = None,
                  cb: Int = currBranches): Value = {

      stm match {
        case ExpStm(e) => interpAExp(e, env)
        case AssignStm(v, e) =>
          val r = interpAExp(e, env)
          env.update(v, r)
          r
        case CompStm(s1, s2) =>
          interpStm(s1, env, next = Some(s2)) //We set the value next to s2, to allow proper forking in ifstm and whilestm
          interpStm(s2, env)
        case IfStm(c, thenStm, elseStm) =>
          val cond = interpBExp(c, env)
          cond match {
            case BoolValue(b) => if (b) interpStm(thenStm, env) else interpStm(elseStm, env)
            case SymValue(e: BoolExpr) =>
              val f = ctx.mkAnd(pc.formula, e)
              val fNeg = ctx.mkAnd(pc.formula, ctx.mkNot(e))
              val s = ctx.mkSolver()
              val thenBranch = s.check(f)
              val elseBranch = s.check(fNeg)
              if (thenBranch == Status.SATISFIABLE) {
                if (elseBranch == Status.SATISFIABLE) {
                  if (cb < maxBranches) {
                    next match {
                      case None =>
                        interpProg(Prog(p.funcs, elseStm), env.clone(), pc.forkPathConstraint(fNeg), symbols.clone(), maxBranches, currBranches = cb + 1)
                      case Some(statement) =>
                        val pr = Prog(p.funcs, CompStm(elseStm, statement))
                        interpProg(pr, env.clone(), pc.forkPathConstraint(fNeg), symbols.clone(), maxBranches, currBranches = cb + 1)
                    }
                  } else {
                    println("max branches reached - ignoring further branches")
                  }
                } else if (elseBranch == Status.UNSATISFIABLE) {
                  println(s"Else branch not satisfiable: $fNeg")
                } else {
                  println(s"Satisfiabiliy of else branch unknown: $fNeg")
                }
                pc.addConstraint(f)
                interpStm(thenStm, env, next, if (cb < maxBranches) cb + 1 else cb)

              } else if (thenBranch == Status.UNSATISFIABLE) {
                println(s"Then branch not satisfiable: $f")
                if (elseBranch == Status.SATISFIABLE) {
                  pc.addConstraint(fNeg)
                  interpStm(elseStm, env, next, cb)
                } else if (elseBranch == Status.UNSATISFIABLE) {
                  throw new IllegalStateException(s"Current state is infeasible, both branches unsatisfiable:  \n" +
                    s"Then branch PC: $f \n" +
                    s"else branch PC: $fNeg"
                  )
                } else {
                  println(
                    s"Else branch satisfiability unknown: $fNeg \n" +
                      "Dont know how to continue"
                  )
                  ErrorValue()
                }
              } else {
                // Then branch should be unknown
                println(s"Then branch satisfiability unknown: $f")
                if (elseBranch == Status.SATISFIABLE) {
                  pc.addConstraint(fNeg)
                  interpStm(elseStm, env, next, cb)
                } else if (elseBranch == Status.UNSATISFIABLE) {
                  println(
                    s"Else branch not satisfiable: $fNeg \n" +
                      "Dont know how to continue"
                  )
                  ErrorValue()
                } else {
                  // both should be unknown
                  println(
                    s"Else branch satisfiability unknown: $fNeg \n" +
                      "Dont know how to continue"
                  )
                  ErrorValue()
                }
              }
          }

        case ws: WhileStm => interpWhile(ws, env, next, cb)
        case _ => throw new UnsupportedOperationException("method not implemented")
      }
    }
    //TODO refactor this method and the code for if statements, to avoid duplicate code
    /**
      * @param ws   the while statement to be interpreted
      * @param env  current version of our environment(mapping from vars to symbolic values)
      * @param next the expression to be executed after this while statement (None if there is nothing to be executed, Some(stm) if there is
      * @return a Value (specifcally either Unit or SymValue)
      */
    def interpWhile(ws: Grammar.Stm.WhileStm, env: mutable.HashMap[Var, SymValue], next: Option[Stm], cb: Int): Value = {
      def dowork(): Value = {
        val cond = interpBExp(ws.c, env)
        cond match {
          case BoolValue(b) => if (b) {
            interpStm(ws.doStm, env)
            dowork()
          } else Unit()
          case SymValue(e: BoolExpr) =>
            val f = ctx.mkAnd(pc.formula, e)
            val fNeg = ctx.mkAnd(pc.formula, ctx.mkNot(e))
            val s = ctx.mkSolver()
            val continueBranch = s.check(f)
            val endBranch = s.check(fNeg)
            if (endBranch == Status.SATISFIABLE) {
              if (continueBranch == Status.SATISFIABLE) {
                if (cb < maxBranches) {
                  val pr = next match {
                    case None => Prog(p.funcs, CompStm(ws.doStm, ws))
                    case Some(stm) => Prog(p.funcs, CompStm(ws.doStm, CompStm(ws, stm)))
                  }
                  interpProg(pr, env.clone(), pc.forkPathConstraint(f), symbols.clone(), maxBranches, currBranches = cb + 1)
                } else {
                  println("max branches reached - ignoring further loops")
                }
              } else if (continueBranch == Status.UNSATISFIABLE) {
                println(s"Continue branch not satisfiable: $f")
              } else {
                println(s"Continue branch satisfiability unknown: $f")
              }
              pc.addConstraint(fNeg)
              next match {
                case None => Unit()
                case Some(stm) => interpStm(stm, env, next, if (cb < maxBranches) cb + 1 else cb)
              }
            } else if (endBranch == Status.UNSATISFIABLE) {
              println(s"End branch not satisfiable: $fNeg")
              if (continueBranch == Status.SATISFIABLE) {
                if (cb < maxBranches) {
                  val pr = next match {
                    case None => Prog(p.funcs, CompStm(ws.doStm, ws))
                    case Some(stm) => Prog(p.funcs, CompStm(ws.doStm, CompStm(ws, stm)))
                  }
                  interpProg(pr, env.clone(), pc.forkPathConstraint(f), symbols.clone(), maxBranches, currBranches = cb + 1)
                } else {
                  println("max branches reached - ignoring further loops")
                  Unit()
                }
              } else if (continueBranch == Status.UNSATISFIABLE) {
                throw new IllegalStateException(
                  "Current state is infeasible, Both branches are unsatisfiable.\n" +
                    s"End branch PC: $fNeg \n" +
                    s"Continue branch PC: $f"
                )
              } else {
                println(
                  s"Continue branch satisfiability is unknown: $f \n" +
                    "Dont know how to continue"
                )
                ErrorValue()
              }

            } else {
              if (continueBranch == Status.SATISFIABLE) {
                if (cb < maxBranches) {
                  val pr = next match {
                    case None => Prog(p.funcs, CompStm(ws.doStm, ws))
                    case Some(stm) => Prog(p.funcs, CompStm(ws.doStm, CompStm(ws, stm)))
                  }
                  interpProg(pr, env.clone(), pc.forkPathConstraint(f), symbols.clone(), maxBranches, currBranches = cb + 1)
                } else {
                  println("max branches reached - ignoring further loops")
                  Unit()
                }
              } else if (continueBranch == Status.UNSATISFIABLE) {
                println(
                  s"Continue branch satisfiability is unknown: $f \n" +
                  "Dont know how to continue"
                )
                ErrorValue()
              } else {
                println(
                  "Both branches satisfiability is unknown \n" +
                  s"End branch: $fNeg" +
                  s"Continue branch: $f \n" +
                  "Dont know how to continue"
                )
                ErrorValue()
              }
            }
        }
      }

      dowork()
    }

    /**
      * @param e   the expression to be interpreted
      * @param env the current version of the environment(mapping from vars to symbolic values)
      * @return a SymValue which is an arithmetic expressions over integers and symbols
      */
    def interpAExp(e: AExp, env: mutable.HashMap[Var, SymValue]): SymValue = {

      e match {
        case Integer(i) => SymValue(ctx.mkInt(i));
        case Sym(sym) =>
          val const = ctx.mkIntConst(sym)
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
            // we simply map all arithmetic expressions to equivalent expressions as used by z3
            case Plus() => SymValue(ctx.mkAdd(v1, v2))
            case Sub() => SymValue(ctx.mkSub(v1, v2))
            case Mul() => SymValue(ctx.mkMul(v1, v2))
            case Div() => SymValue(ctx.mkDiv(v1, v2))
          }
        case CallExp(name, args) =>
          //TODO refactor this to single fold
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
            case GtOp() => SymValue(ctx.mkGt(v1, v2))
            case EqOp() => SymValue(ctx.mkEq(v1, v2))
          }
      }
    }

    val res = interpStm(p.stm, env)
    val s = ctx.mkSolver()
    s.add(pc.formula)
    val model = {
      s.check()
      s.getModel
    }
    val concreteValSet = modelToSet(model, symbols)
    val msg = "******************************* \n" +
      s"execution returned: ${
        res match {
          case SymValue(e) => e.simplify()
          case _ => res
        }
      } \n" +
      s"path constraint is: ${pc.formula} \n" +
      s"model satisfying pc is: $concreteValSet \n" +
      "******************************* \n"
    println(msg)
    res
  }

  /**
    * function to return a string representation of a model(that is concrete values) that satisfies the model.
    *
    * @param m       the model that z3 returns based on the constraints in the path constraint
    * @param symbols a set containing all symbols that are used in the execution
    * @return a string representation of the model
    */
  def modelToSet(m: Model, symbols: mutable.Set[IntExpr]) = {
    val symValPairs = symbols.map((sym: IntExpr) => (sym.getSExpr, m.eval(sym, false)))
    symValPairs
  }

}


//TODO Refactor sat/unsat/unknown og tilføj pre/post condition samt assertions. split values op og tilføj error.

