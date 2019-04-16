package interpreters

import com.microsoft.z3._
import grammars.SymbolicGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.SymbolicGrammar.BOp.{Eq, Gt}
import grammars.SymbolicGrammar.Exp._
import grammars.SymbolicGrammar.Input.{Concrete, Symbolic}
import grammars.SymbolicGrammar.{AOp, BOp, Exp, Id, Prog, Result, SymbolicValue}
import grammars.SymbolicGrammar.Result._
import grammars.SymbolicGrammar.SymbolicValue.{Constraint, IntValue, SymExpr, UnitValue}

import scala.collection.mutable

class SymbolicInterpreter(maxForks: Int = 10) {

  def interpProg(p: Prog, env: mutable.HashMap[Id, SymbolicValue],
                 initPC: List[Constraint] = List.empty[Constraint],
                 ctx: Context,
                 initForks: Int = 0): List[(List[Constraint], Result)] = {

    //We allow for some local state to keep track of current path constraint, and accumulating results from other paths
    var resList = List.empty[(List[Constraint], Result)]
    var pc = initPC
    var currForks = initForks

    def interpExp(e: Exp, env: mutable.HashMap[Id, SymbolicValue], next: Option[Exp] = None): Result = e match {
      case Lit(v) => v match {
        // we map integer literals to integer constants in Z3 to simplify evaluation of arithmetic expressions.
        case IntValue(value) => Ok(SymExpr(ctx.mkInt(value)))

        case _ => Ok(v)
      }
      case Var(id) => env.get(id) match {
        case None => Error(s"variable ${id.s} not defined")

        case Some(value) => Ok(value)
      }
      case AExp(e1, e2, op) =>
        val error = Error("arithmetic operations on non integer value(s)")
        interpExp(e1, env).filterWithDefault(error)(_.isInstanceOf[SymExpr])
          .map2(interpExp(e2, env).filterWithDefault(error)(_.isInstanceOf[SymExpr]))(handleAExp(_, _, op))

      case BExp(e1, e2, op) =>
        val error = Error("comparison of integer value(s)")
        interpExp(e1, env).filterWithDefault(error)(_.isInstanceOf[SymExpr])
          .map2(interpExp(e2, env).filterWithDefault(error)(_.isInstanceOf[SymExpr]))(handleBExp(_, _, op))
      case AssignExp(v, exp) => handleAssignment(exp, v.id, env)
      case IfExp(c, thenExp, elseExp) =>
        val elsExp = next match {
          case None => elseExp
          case Some(exp) => ComExp(elseExp, exp)
        }
        interpExp(c, env).flatMap(handleConditional(_, elsExp, thenExp, env))
      case we: WhileExp =>
        val contExp = next match {
          case None => ComExp(we.doExp, we)
          case Some(exp) => ComExp(we.doExp, ComExp(we, exp))
        }
        interpExp(we.c, env).flatMap(handleConditional(_, Lit(UnitValue()), contExp, env))

      case ComExp(e1, e2) =>
        //We set next to e2, in case we need to fork. this way, we can preserve the control flow across calls to interpProg
        interpExp(e1, env, next = Some(e2)).map2(interpExp(e2, env))((_, r2) => r2)
      case CallExp(id, args) => p.funcs.get(id.s) match {
        case None => Error(s"Undefined function")
        case Some(f) =>
          if (args.length == f.params.length) {
            val localEnv = env.clone()
            args.map({ case Concrete(exp) => exp case Symbolic(s) => Lit(SymExpr(ctx.mkIntConst(s))) })
              .zip(f.params).map(p => handleAssignment(p._1, p._2, localEnv)).find(_.isInstanceOf[Error]) match {
              case Some(err) => err
              case None => interpExp(f.body, localEnv)
            }
          }
          else
            Error("formal and actual parameter list differ in length")

      }
      case _ => throw new Exception("not implemented")
    }

    def handleAExp(s1: SymbolicValue, s2: SymbolicValue, op: AOp): SymbolicValue = {
      val v1 = s1.asInstanceOf[SymExpr].e
      val v2 = s2.asInstanceOf[SymExpr].e
      op match {
        case Add() => SymExpr(ctx.mkAdd(v1, v2))
        case Sub() => SymExpr(ctx.mkSub(v1, v2))
        case Mul() => SymExpr(ctx.mkMul(v1, v2))
        case Div() => SymExpr(ctx.mkDiv(v1, v2))
      }
    }

    def handleBExp(s1: SymbolicValue, s2: SymbolicValue, op: BOp): SymbolicValue = {
      val v1 = s1.asInstanceOf[SymExpr].e
      val v2 = s2.asInstanceOf[SymExpr].e
      op match {
        case Gt() => Constraint(ctx.mkGt(v1, v2))
        case Eq() => Constraint(ctx.mkEq(v1, v2))
      }
    }

    def handleAssignment(exp: Exp, id: Id, env: mutable.HashMap[Id, SymbolicValue]): Result =
      interpExp(exp, env).filterWithDefault(Error("assignment of non integer value(s)"))(_.isInstanceOf[SymExpr])
        .flatMap(r => {
          env.update(id, r)
          Ok(r)
        })

    def checkSat(pc: List[Constraint]): Status =
      ctx.mkSolver().check(pc.foldLeft(ctx.mkTrue())((acc, c) => ctx.mkAnd(acc, c.c)))

    def checkBranches(br1: Constraint, br2: Constraint, br1Exp: Exp, br2Exp: Exp, env: mutable.HashMap[Id, SymbolicValue]): Result = {

      val br1Status = checkSat(pc :+ br1)
      val br2Status = checkSat(pc :+ br2)
      br2Status match {
        case Status.UNSATISFIABLE => br1Status match {
          case Status.UNSATISFIABLE => Error("Both branches unsatisfiable")
          case _ =>
            pc = pc :+ br1
            interpExp(br1Exp, env)
        }
        case _ => br1Status match {
          case Status.UNSATISFIABLE =>
            pc = pc :+ br2
            interpExp(br2Exp, env)
          case _ =>
            resList =
              if (currForks <= maxForks) {
                currForks += 1
                resList ++ interpProg(Prog(p.funcs, br2Exp), env.clone(), initPC = pc :+ br2, ctx, currForks)
              }
              else {
                resList ++ List((pc :+ br2, Error("max forks reached, ignoring path")))
              }
            pc = pc :+ br1
            interpExp(br1Exp, env)
        }
      }
    }

    def handleConditional(v: SymbolicValue, e1: Exp, e2: Exp, env: mutable.HashMap[Id, SymbolicValue]): Result = v match {
      case SymExpr(e) =>
        // potential infinite loop here?
        val br1 = Constraint(ctx.mkNot(ctx.mkEq(e, ctx.mkInt(0))))
        val br2 = Constraint(ctx.mkEq(e, ctx.mkInt(0)))
        checkBranches(br1, br2, e1, e2, env)

      case Constraint(c) => checkBranches(Constraint(ctx.mkNot(c)), Constraint(c), e1, e2, env)

      case UnitValue() => Error("unit value as conditional")
    }

    val r =
      interpExp(p.e, env).map({
        case Constraint(e) => Constraint(e.simplify().asInstanceOf[BoolExpr])

        case SymExpr(e) => SymExpr(e.simplify().asInstanceOf[ArithExpr])

        case v => v
      })
    resList :+ (pc, r)
  }
}
