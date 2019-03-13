import java.util

import Grammar.AExp._
import Grammar.Aop._
import Grammar.BExp._
import Grammar.Bop._
import Grammar.Stm._
import Grammar.SymbolicValue.AValue.ErrorValue
import Grammar.SymbolicValue._
import Grammar.{SymbolicValue, _}
import com.microsoft.z3._

import scala.collection.mutable


class SymbolicInterpreter(ctx: Context = new Context(new util.HashMap[String, String])) {
  def interpProg(p: Prog,
                 env: mutable.HashMap[Var, AValue.Exp] = mutable.HashMap(),
                 pc: PathConstraint = new PathConstraint(ctx, ctx.mkTrue()),
                 symbols: mutable.Set[IntExpr] = mutable.Set(),
                 maxBranches: Int,
                 currBranches: Int = 1): SymbolicValue = {


    def interpStm(stm: Stm,
                  env: mutable.HashMap[Var, AValue.Exp],
                  next: Option[Stm] = None,
                  cb: Int = currBranches): SymbolicValue = {

      stm match {
        case ExpStm(e) => interpAExp(e, env)
        case AssignStm(v, e) =>
          val r = interpAExp(e, env)
          r match {
            case AValue.ErrorValue() => r
            case exp: AValue.Exp =>
              env.update(v, exp)
              exp
          }
        case CompStm(s1, s2) =>
          val res = interpStm(s1, env, next = Some(s2)) //We set the value next to s2, to allow proper forking in ifstm and whilestm
          //If we encounter an error during interp of s1, we should just end this execution. 
          res match {
            case SymbolicValue.AValue.ErrorValue() => res
            case SymbolicValue.BValue.ErrorValue() => res
            case _ =>
              interpStm(s2, env)
          }
        case IfStm(c, thenStm, elseStm) =>
          val cond = interpBExp(c, env)
          cond match {
            case BValue.ErrorValue() => cond
            case BValue.Exp(e) =>
              if (e.isTrue) interpStm(thenStm, env)
              else if (e.isFalse) interpStm(elseStm, env)
              else {
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
                    AValue.ErrorValue()
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
                    AValue.ErrorValue()
                  } else {
                    // both should be unknown
                    println(
                      s"Else branch satisfiability unknown: $fNeg \n" +
                        "Dont know how to continue"
                    )
                    AValue.ErrorValue()
                  }
                }
              }
          }
        case ws: WhileStm => interpWhile(ws, env, next, cb)
        case AssertStm(c) =>
          val cond = interpBExp(c, env)
          cond match {
            case BValue.ErrorValue() => cond
            case BValue.Exp(e) =>
              // to check assertion x > 2, we construct the negation x <= 2 and asks if this is satisfiable.
              val f = ctx.mkAnd(pc.formula, ctx.mkNot(e))
              val s = ctx.mkSolver()
              val assertRes = s.check(f)
              assertRes match {
                case Status.SATISFIABLE =>
                  val model = s.getModel
                  println(
                    s"Assertion violation: $e \n" +
                      s"model that causes assertion violation: ${modelToSet(model, symbols).toString()}"
                  )
                  AValue.ErrorValue()
                case Status.UNSATISFIABLE => Unit()
                case Status.UNKNOWN =>
                  println("Cannot determine if assertion is violated")
                  AValue.ErrorValue()
              }
          }
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
    def interpWhile(ws: Grammar.Stm.WhileStm, env: mutable.HashMap[Var, AValue.Exp], next: Option[Stm], cb: Int): SymbolicValue = {
      def dowork(): SymbolicValue = {
        val cond = interpBExp(ws.c, env)
        cond match {
          case BValue.ErrorValue() => cond
          case BValue.Exp(e) =>
            if (e.isTrue) {
              interpStm(ws.doStm, env)
              dowork()
            } else if (e.isFalse) {
              Unit()
            } else {
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
                  AValue.ErrorValue()
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
                  AValue.ErrorValue()
                } else {
                  println(
                    "Both branches satisfiability is unknown \n" +
                      s"End branch: $fNeg" +
                      s"Continue branch: $f \n" +
                      "Dont know how to continue"
                  )
                  AValue.ErrorValue()
                }
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
    def interpAExp(e: AExp, env: mutable.HashMap[Var, AValue.Exp]): AValue = {

      e match {
        case Integer(i) => AValue.Exp(ctx.mkInt(i));
        case Sym(sym) =>
          val const = ctx.mkIntConst(sym)
          symbols.add(const)
          AValue.Exp(const)
        case v: Var =>
          val opt = env.get(v)
          opt match {
            case None => throw new NoSuchElementException(s"variable ${v.name} not defined")
            case Some(value) => value
          }
        case bin: AExp.BinExp =>
          val e1 = interpAExp(bin.e1, env)
          val e2 = interpAExp(bin.e2, env)
          e1 match {
            case AValue.ErrorValue() => e1
            case AValue.Exp(v1) =>
              e2 match {
                case AValue.ErrorValue() => e2
                case AValue.Exp(v2) =>
                  bin.op match {
                    // we simply map all arithmetic expressions to equivalent expressions as used by z3
                    case Plus() => AValue.Exp(ctx.mkAdd(v1, v2))
                    case Sub() => AValue.Exp(ctx.mkSub(v1, v2))
                    case Mul() => AValue.Exp(ctx.mkMul(v1, v2))
                    case Div() => AValue.Exp(ctx.mkDiv(v1, v2))
                  }
              }
          }
        case CallExp(name, args) =>
          //TODO refactor this to single fold
          val d = p.funcs.get(name)
          d match {
            case None => throw new NoSuchElementException(s"function $name  not defined")
            case Some(f) =>
              val vals = args.map((exp: AExp) => interpAExp(exp, env))
              if ( vals.exists(_.isInstanceOf[AValue.ErrorValue]) ) return AValue.ErrorValue()
              val argValPairs = f.args zip vals
              val addTo = (m: mutable.HashMap[Var, AValue.Exp], t: (Var, AValue)) => {
                m.update(t._1, t._2.asInstanceOf[AValue.Exp])
                m
              }
              val venvCopy = env.clone()
              val newEnv = argValPairs.foldLeft(venvCopy)(addTo)
              f.preCondition match {
                case Some(c) =>
                  val cond = interpBExp(c, newEnv)
                  cond match {
                    case BValue.ErrorValue() => cond
                    case BValue.Exp(exp) =>
                      val fNeg = ctx.mkAnd(pc.formula, ctx.mkNot(exp))
                      val s = ctx.mkSolver()
                      val checkPreCond = s.check(fNeg)
                      if (checkPreCond == Status.SATISFIABLE) {
                        val model = s.getModel
                        println(
                          s"Precondition violation: $exp \n" +
                            s"Model breaking precondtion: ${modelToSet(model, symbols).toString()}")
                        return AValue.ErrorValue()
                      }
                  }
                // Check whether negation of cond is satisfiable. If it is, report back error along with input that violates the precond.
                // If pre condition can not be violated, should we include cond in PC, or is it already implied?
                case None => // Do nothing. No conditions to check.
              }
              val res = interpStm(f.fbody, newEnv)
              res match {
                case sv: AValue => sv
                case _ => throw new IllegalArgumentException("functions must return symbolic expressions")
              }
          }
        case _ => throw new UnsupportedOperationException("method not implemented")
      }
    }


    def interpBExp(e: BExp, env: mutable.HashMap[Var, AValue.Exp]): BValue = {
      e match {
        case Bool(b) => BValue.Exp(ctx.mkBool(b))
        case BExp.BinExp(e1, e2, op) =>
          val exp1 = interpAExp(e1, env)
          val exp2 = interpAExp(e2, env)
          exp1 match {
            case AValue.ErrorValue() => BValue.ErrorValue()
            case AValue.Exp(v1) =>
              exp2 match {
                case AValue.ErrorValue() => BValue.ErrorValue()
                case AValue.Exp(v2) =>
                  op match {
                    case GtOp() => BValue.Exp(ctx.mkGt(v1, v2))
                    case EqOp() => BValue.Exp(ctx.mkEq(v1, v2))
                  }
              }
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
          case AValue.Exp(e) => e.simplify()
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

