import com.microsoft.z3.Context
import grammars.SymbolicGrammar.AOp.Add
import grammars.SymbolicGrammar.Exp.{AExp, Lit}
import grammars.SymbolicGrammar.SymbolicValue.IntValue
import grammars.SymbolicGrammar.{FDecl, Id, Prog, SymbolicValue}
import interpreters.SymbolicInterpreter
import z3.scala.{Z3Config, Z3Context}

import scala.collection.immutable.HashMap
import scala.collection.mutable

object SymDriver extends App {
  val interpreter = new SymbolicInterpreter()

  val test = Prog(
    HashMap[String, FDecl](),
    AExp(Lit(IntValue(1)), Lit(IntValue(7)), Add())
  )

  val ctx = new Context()
  print(interpreter.interpProg(test, new mutable.HashMap[Id, SymbolicValue](), ctx))
}
