import grammars.ConcreteGrammar.AOp.{Add, Mul}
import grammars.ConcreteGrammar.BOp.{Eq, Gt}
import grammars.ConcreteGrammar.ConcreteValue.IntValue
import grammars.ConcreteGrammar.Exp._
import grammars.ConcreteGrammar.Input.Concrete
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
        Lit(IntValue(1)),
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

  val dec = FDecl(Id("test"), List(Id("x"), Id("y"), Id("z")), Lit(IntValue(2)))
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
    CallExp(Id("test"), List(Concrete(Lit(IntValue(1))), Concrete(Lit(IntValue(2))), Concrete(Lit(IntValue(3))))),
  )

  val test5 = Prog(
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
    CallExp(Id("test"), List(Concrete(Lit(IntValue(3))), Concrete(Lit(IntValue(7)))))
  )

  print(concreteInterpreter.interpProg(test5, new mutable.HashMap[Id, ConcreteValue]()))

}
