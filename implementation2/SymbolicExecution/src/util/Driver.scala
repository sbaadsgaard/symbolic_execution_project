package util

import grammars.ConcreteGrammar.AOp.{Add, Mul}
import grammars.ConcreteGrammar.BOp.{Eq, Gt}
import grammars.ConcreteGrammar.ConcreteValue.IntValue
import grammars.ConcreteGrammar.Exp._
import grammars.ConcreteGrammar._
import interpreters.ConcreteInterpreter

import scala.collection.immutable.HashMap

object Driver extends App {


  val concreteInterpreter = new ConcreteInterpreter()
  /*
  val test = Prog(new HashMap[String, FDecl](), AExp(Lit(IntValue(1)), Lit(IntValue(0)), Add()))
  val test1 = Prog(new HashMap[String, FDecl](),
    SeqExp(
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
    SeqExp(
      AssignExp(Var(Id("x")), Lit(IntValue(0))),
      SeqExp(
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

  val test4 = Prog(
    HashMap("test" -> FDecl(Id("test"), List(Id("x"), Id("y"), Id("z")),
      AExp(
        Var(Id("x")),
        AExp(
          Var(Id("y")),
          Var(Id("z")),
          Add()
        ),
        Add()
      )
    )),
    CallExp(Id("test"), List(Lit(IntValue(1)), Lit(IntValue(2)), Lit(IntValue(3))))
  )

  val test5 = Prog(
    HashMap("test" -> FDecl(
      Id("test"),
      List(Id("x"), Id("y")),
      SeqExp(
        AssignExp(Var(Id("rev")), AExp(Var(Id("x")), Lit(IntValue(2)), Mul())),
        IfExp(
          BExp(Var(Id("rev")), Var(Id("y")), Gt()),
          Var(Id("rev")),
          Var(Id("y"))
        )
      )
    )),
    CallExp(Id("test"), List(Lit(IntValue(11)), Lit(IntValue(7))))
  )


  val test6 = Prog(
    HashMap("test" -> FDecl(
      Id("test"),
      List.empty[Id],
      AssignExp(Var(Id("x")), Lit(IntValue(10)))
    )),
    SeqExp(
      AssignExp(Var(Id("x")), Lit(IntValue(42))),
      SeqExp(
        CallExp(
          Id("test"),
          List.empty[Exp]
        ),
        Var(Id("x"))
      )
    )
  )

  print(concreteInterpreter.interpExp(test6, test6.e, HashMap[Id, ConcreteValue]()))
  */
}
