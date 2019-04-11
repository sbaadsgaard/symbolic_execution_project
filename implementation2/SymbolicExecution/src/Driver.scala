
import grammars.ConcreteGrammar.AOp.Add
import grammars.ConcreteGrammar.Bop.{Eq, Gt}
import grammars.ConcreteGrammar.ConcreteValue.{BoolValue, IntValue, UnitValue}
import grammars.ConcreteGrammar.Exp.{AExp, AssignExp, BExp, CallExp, ComExp, IfExp, Lit, Var, WhileExp}
import grammars.ConcreteGrammar._
import interpreters.ConcreteInterpreter

import scala.collection.immutable.HashMap
import scala.collection.mutable

object Driver extends App {
  val concreteInterpreter = new ConcreteInterpreter()
  val test = Prog(new HashMap[String, FDecl](), AExp(Lit(IntValue(1)), Lit(IntValue(2)), Add()))
  val test1 = Prog(new HashMap[String, FDecl](),
    ComExp(
      AssignExp(Var("x"), AExp(Lit(IntValue(1)), Lit(IntValue(11)), Add())),
      Var("x")
    ))
  val test2 = Prog(new HashMap[String, FDecl](),
    IfExp(
      BExp(
        Lit(IntValue(2)),
        Lit(IntValue(2)),
        Eq()
      ),
      Lit(IntValue(2)),
      Lit(IntValue(3))))

  val test3 = Prog(new HashMap[String, FDecl](),
    ComExp(
      AssignExp(Var("x"), Lit(IntValue(0))),
      ComExp(
        WhileExp(
          BExp(
            Lit(IntValue(10)),
            Var("x"),
            Gt()
          ),
          AssignExp(
            Var("x"),
            AExp(
              Var("x"),
              Lit(IntValue(1)),
              Add()
            )
          )
        ),
        Var("x")
      )
    )
  )

  val test4 = Prog(
    HashMap("test" -> FDecl(
      "test",
      List(Var("x"), Var("y"), Var("z")),
      AExp(Var("x"), AExp(Var("y"), Var("z"), Add()), Add())
    )),
    CallExp("test", List(AExp(Lit(IntValue(1)), Lit(IntValue(2)), Add()), Lit(IntValue(2)), Lit(IntValue(3))))
  )
  print(concreteInterpreter.interpProg(test4, new mutable.HashMap[Var, ConcreteValue]()))

}
