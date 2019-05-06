package util

import grammars.SymbolicGrammar.AOp.{Add, Mul, Sub}
import grammars.SymbolicGrammar.BOp.{Geq, Gt, Lt}
import grammars.SymbolicGrammar.Exp._
import grammars.SymbolicGrammar.Stm.{AssertStm, AssignStm, ExpStm, IfStm, SeqStm}
import grammars.SymbolicGrammar.SymbolicInt.{IntValue, Symbol}
import grammars.SymbolicGrammar.SymbolicValue.UnitValue
import grammars.SymbolicGrammar._
import interpreters.SymbolicInterpreter

import scala.collection.immutable.HashMap

object SymDriver extends App {

  val interpreter = new SymbolicInterpreter(maxForks = 3)

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

   */
  // test if we ever execute invalid combination of function arguments. seems ok, but we do have some duplication in the path constraint
  /*
    function test(s) {
      if (s > 2) {
        t1 = 1
      } else {
        t1 = 2
      }
      if (s < 2) {
        t2 = 3
      } else {
        t2 = 5
      }
      test1(t1, t2)
    }

    function test1(v1, v2) {
      v1 + v2
    }

    test(symbol(s))
   */


  val t6 = Prog(
    HashMap(
      Id("test") -> FDecl(Id("Test"), List(Id("s")),
          SeqStm(
            IfStm(
              BExp(Var(Id("s")), Lit(IntValue(2)), Gt()),
              AssignStm(Var(Id("t1")), Lit(IntValue(1))),
              AssignStm(Var(Id("t1")), Lit(IntValue(2)))
            ),
            SeqStm(
              IfStm(
                BExp(Var(Id("s")), Lit(IntValue(2)), Lt()),
                AssignStm(Var(Id("t2")), Lit(IntValue(3))),
                AssignStm(Var(Id("t2")), Lit(IntValue(5)))),
              ExpStm(
                CallExp(Id("test1"), List(Var(Id("t1")), Var(Id("t2"))))
              )
            )
          )
      ),
      Id("test1") -> FDecl(Id("test1"), List(Id("v1"), Id("v2")),
        ExpStm(
          AExp(Var(Id("v1")), Var(Id("v2")), Add())
        )
      )
    ),
    CallExp(Id("test"), List(Lit(Symbol("s"))))
  )

  val t7 = Prog(
    HashMap(
      Id("computeRevenue") -> FDecl(Id("computeRevenue"), List(Id("cost"), Id("units")),
        SeqStm(
          AssignStm(Var(Id("revenue")), AExp(Var(Id("units")), Lit(IntValue(2)), Mul())),
          SeqStm(
            IfStm(
              BExp(Var(Id("revenue")), Lit(IntValue(16)), Geq()),
              SeqStm(
                AssignStm(Var(Id("revenue")), AExp(Var(Id("revenue")), Lit(IntValue(10)), Sub())),
                AssertStm(BExp(Var(Id("revenue")), Var(Id("cost")), Geq()))
              ),
              ExpStm(Lit(UnitValue()))
            ),
            ExpStm(Var(Id("revenue")))
          )
        )
      )
    ),
    CallExp(Id("computeRevenue"), List(Lit(Symbol("c")), Lit(Symbol("u"))))
  )

  interpreter.interpProg(t7)
    .foreach(r => println(Util.prettyPrintExpRes(r)))

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
