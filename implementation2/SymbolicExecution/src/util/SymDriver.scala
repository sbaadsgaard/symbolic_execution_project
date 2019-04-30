package util

import grammars.SymbolicGrammar.AOp.Add
import grammars.SymbolicGrammar.BOp.Leq
import grammars.SymbolicGrammar.Exp._
import grammars.SymbolicGrammar.Stm.{AssignStm, IfStm, SeqStm}
import grammars.SymbolicGrammar.SymbolicInt.{IntValue, Symbol}
import grammars.SymbolicGrammar.SymbolicValue.UnitValue
import grammars.SymbolicGrammar._
import interpreters.{PathConstraint, SymbolicInterpreter}

import scala.collection.immutable.HashMap

object SymDriver extends App {

  val interpreter = new SymbolicInterpreter(maxForks = 3)

  val util = new Util()

  /*
  val t = Prog(
    HashMap[String, FDecl](),
    AExp(Lit(Symbol("a")), Lit(Symbol("b")), Add())
  )

  val t1 = Prog(
    HashMap[String, FDecl](),
    AExp(
      IfExp(
        BExp(Lit(Symbol("s")), Lit(IntValue(2)), Geq()),
        Lit(IntValue(1)),
        Lit(IntValue(2))
      ),
      Lit(IntValue(3)),
      Add()))

  val t2 = Prog(
    HashMap[String, FDecl](),
    AssignExp(Var(Id("x")), Lit(UnitValue())))


  val t3 = Prog(
    HashMap[String, FDecl](),
    SeqExp(
      AssignExp(
        Var(Id("x")),
        IfExp(
          BExp(
            Lit(Symbol("s")),
            Lit(IntValue(3)),
            Geq()
          ),
          Lit(IntValue(5)),
          Lit(IntValue(11))
        )
      ),
      AExp(Var(Id("x")), Lit(IntValue(2)), Mul())
    )
  )

  val t4 = Prog(
    HashMap[String, FDecl](),
    SeqExp(
      AssignExp(Var(Id("a")), Lit(Symbol("a"))),
      SeqExp(
        WhileExp(
          BExp(Var(Id("a")), Lit(IntValue(2)), Leq()),
          AssignExp(Var(Id("a")), AExp(Var(Id("a")), Lit(IntValue(1)), Add()))
        ),
        Var(Id("a"))
      )
    )
  )
  */
  val t5 = Prog(
    HashMap(Id("test") -> FDecl(Id("test"), List(Id("x"), Id("y")),
      FBody(
        SeqStm(
          AssignStm(
            Var(Id("res")),
            Lit(IntValue(0))
          ),
          IfStm(
            BExp(
              Var(Id("x")),
              AExp(Var(Id("y")), Lit(IntValue(2)), Add()),
              Leq()
            ),
            AssignStm(Var(Id("res")), Lit(UnitValue())),
            AssignStm(Var(Id("res")), Lit(IntValue(3)))
          )
        ),
        Var(Id("res"))))
    ),
    CallExp(Id("test"), List(Lit(Symbol("x")), Lit(Symbol("y"))))
  )


  interpreter.interpProg(t5)
    .foreach(println(_))
  /*
  interpreter.interpExp(t4 , t4.e, HashMap[Id, SymbolicValue](), PathConstraint(List.empty[SymbolicBool]))
     .foreach(println(_))

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

   val test5 = Prog(
     HashMap("test" -> FDecl(
       Id("test"),
       List(Id("x")),
       IfExp(
         BExp(Var(Id("x")), Lit(IntValue(2)), Gt()),
         Lit(IntValue(0)),
         Lit(IntValue(1))
       )
     )),
     ComExp(
       AssignExp(Var(Id("y")), CallExp(Id("test"), List(Symbolic("x")))),
       Var(Id("y"))
     )
   )

   interpreter.interpProg(test5, new mutable.HashMap[Id, SymbolicValue](), List.empty[Constraint], ctx)
     .map(p => s"pc: ${p._1.map(_.c.simplify().asInstanceOf[BoolExpr])}  res: ${p._2}").foreach(println)

    */

}
