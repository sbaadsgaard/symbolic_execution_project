package util

import com.microsoft.z3
import com.microsoft.z3.BoolExpr
import grammars.SymbolicGrammar.AOp.{Add, Div, Mul, Sub}
import grammars.SymbolicGrammar.BOp.{Eq, Geq, Leq}
import grammars.SymbolicGrammar.SymbolicBool.{False, Not, SymbolicBExp, True}
import grammars.SymbolicGrammar.SymbolicInt.{IntValue, Symbol, SymbolicAExp}
import grammars.SymbolicGrammar.SymbolicValue
import grammars.SymbolicGrammar.SymbolicValue.{SymbolicBool, SymbolicInt, UnitValue}
import interpreters.{PathConstraint, PathResult}
import result.Result

class Util(ctx: z3.Context = new z3.Context()) {
  def translateIntToZ3(v: SymbolicInt): z3.ArithExpr = v match {
    case IntValue(i) => ctx.mkInt(i)
    case Symbol(s) => ctx.mkIntConst(s)
    case SymbolicAExp(s1, s2, op) => op match {
      case Add() => ctx.mkAdd(translateIntToZ3(s1), translateIntToZ3(s2))
      case Sub() => ctx.mkSub(translateIntToZ3(s1), translateIntToZ3(s2))
      case Mul() => ctx.mkMul(translateIntToZ3(s1), translateIntToZ3(s2))
      case Div() => ctx.mkDiv(translateIntToZ3(s1), translateIntToZ3(s2))
    }
  }

  def translateBoolToZ3(b: SymbolicBool): BoolExpr = b match {
    case True() => ctx.mkTrue()
    case False() => ctx.mkFalse()
    case SymbolicBExp(i, j, op) => op match {
      case Leq() => ctx.mkLe(translateIntToZ3(i), translateIntToZ3(j))
      case Geq() => ctx.mkGe(translateIntToZ3(i), translateIntToZ3(j))
      case Eq() => ctx.mkEq(translateIntToZ3(i), translateIntToZ3(j))
    }
    case Not(bool) => ctx.mkNot(translateBoolToZ3(bool))
  }

  def prettyPrintRes(r: Result[SymbolicValue, String]): Result[String, String] = r.map {
    case IntValue(i) => i.toString
    case Symbol(s) => s
    case True() => "true"
    case False() => "false"
    case UnitValue() => "unit"
    case i: SymbolicInt => translateIntToZ3(i).toString
    case b: SymbolicBool => translateBoolToZ3(b).toString
  }

  def prettyPrintPC(pc: PathConstraint): String = s"(${pc.conds.map(translateBoolToZ3).toString}, ${pc.ps})"

  def prettyPrintPathResult(ps: PathResult): String = s"(${prettyPrintPC(ps.pc)}, ${prettyPrintRes(ps.res)})"
}
