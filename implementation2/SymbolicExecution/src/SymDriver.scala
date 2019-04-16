import com.microsoft.z3.{BoolExpr, Context}
import grammars.SymbolicGrammar.Exp.{AExp, AssignExp, BExp, CallExp, ComExp, IfExp, Lit, Var, WhileExp}
import grammars.SymbolicGrammar.AOp.{Add, Mul, Sub}
import grammars.SymbolicGrammar.BOp.Gt
import grammars.SymbolicGrammar.Input.Symbolic
import grammars.SymbolicGrammar.SymbolicValue.{Constraint, IntValue, UnitValue}
import grammars.SymbolicGrammar.{FDecl, Id, Prog, SymbolicValue}
import interpreters.SymbolicInterpreter

import scala.collection.immutable.HashMap
import scala.collection.mutable

object SymDriver extends App {
  val interpreter = new SymbolicInterpreter(maxForks = 3)

  val test = Prog(
    HashMap[String, FDecl](),
    AExp(Lit(IntValue(1)), Lit(IntValue(7)), Add())
  )

  val test1 = Prog(
    HashMap[String, FDecl](),
    BExp(Lit(IntValue(1)), Lit(IntValue(2)), Gt())
  )

  val test2 = Prog(
    HashMap("test" -> FDecl(
      Id("test"),
      List(Id("u"), Id("c")),
      ComExp(
        AssignExp(Var(Id("revenue")), AExp(Var(Id("u")), Lit(IntValue(2)), Mul())),
        IfExp(
          BExp(Var(Id("revenue")), Lit(IntValue(15)), Gt()),
          ComExp(
            AssignExp(Var(Id("revenue")), AExp(Var(Id("revenue")), Lit(IntValue(10)), Sub())),
            IfExp(
              BExp(Var(Id("c")), Var(Id("revenue")), Gt()),
              Lit(UnitValue()),
              Var(Id("revenue"))
            )
          ),
          Var(Id("revenue"))
        )
      )
    )),
    CallExp(Id("test"), List(Symbolic("u"), Symbolic("c")))
  )

  val test3 = Prog(
    HashMap("test" -> FDecl(
      Id("test"),
      List(Id("x"), Id("y")),
      ComExp(
        AssignExp(Var(Id("rev")), AExp(Var(Id("x")), Lit(IntValue(2)), Mul())),
        IfExp(
          BExp(Var(Id("rev")), Var(Id("y")), Gt()),
          Var(Id("x")),
          Var(Id("y"))
        )
      )
    )),
    CallExp(Id("test"), List(Symbolic("x"), Symbolic("y")))
  )

  val test4 = Prog(
    HashMap("test" -> FDecl(
      Id("test"),
      List(Id("a"), Id("b")),
      ComExp(
        AssignExp(Var(Id("r")), Lit(IntValue(1))),
        ComExp(
          AssignExp(Var(Id("i")), Lit(IntValue(0))),
          ComExp(
            WhileExp(
              BExp(Var(Id("b")), Var(Id("i")), Gt()),
              ComExp(
                AssignExp(Var(Id("r")), AExp(Var(Id("r")), Var(Id("a")), Mul())),
                AssignExp(Var(Id("i")), AExp(Var(Id("i")), Lit(IntValue(1)), Add()))
              )
            ),
            Var(Id("r"))
          )
        )
      )
    )),
    CallExp(Id("test"), List(Symbolic("a"), Symbolic("b")))
  )



  val ctx = new Context()
  interpreter.interpProg(test2, new mutable.HashMap[Id, SymbolicValue](), List.empty[Constraint], ctx)
    .map(p => s"pc: ${p._1.map(_.c.simplify().asInstanceOf[BoolExpr])}  res: ${p._2}").foreach(println)
}
