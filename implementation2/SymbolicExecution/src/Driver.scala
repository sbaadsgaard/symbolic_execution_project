import grammars.ConcreteGrammar.AOp.Add
import grammars.ConcreteGrammar.Bop.{Eq, Gt}
import grammars.ConcreteGrammar.ConcreteValue.IntValue
import grammars.ConcreteGrammar.Exp._
import grammars.ConcreteGrammar._
import interpreters.ConcreteInterpreter

import scala.collection.immutable.HashMap
import scala.collection.mutable

object Driver extends App {
  val concreteInterpreter = new ConcreteInterpreter()

  val test = Prog(new HashMap[String, FDecl](), AExp(Lit(IntValue(1)), Lit(IntValue(0)), Add()))
  val test1 = Prog(new HashMap[String, FDecl](),
    ComExp(
      AssignExp(Var(Id("x")), AExp(Lit(IntValue(1)), Lit(IntValue(11)), Add())),
      Var(Id("x"))
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
      AssignExp(Var(Id("x")), Lit(IntValue(0))),
      ComExp(
        WhileExp(
          BExp(
            Lit(IntValue(10)),
            Var(Id("x")),
            Gt()
          ),
          AssignExp(
            Var(Id("x")),
            AExp(
              Var(Id("x")),
              Lit(IntValue(1)),
              Add()
            )
          )
        ),
        Var(Id("x"))
      )
    )
  )
  print(concreteInterpreter.interpProg(test3 , new mutable.HashMap[Id, ConcreteValue]()))

}
