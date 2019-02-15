import Grammar._
import Aop._
import Bop._
import AExp._
import Grammar.Stm._
import Grammar.Prog
import Grammar.FDecl
import scala.collection.mutable

object Driver extends App {

  val concreteInterp = new Interpreter()
  val symbolicInterp = new SymbolicInterpreter()

  /*
   * fun fib(n) = if (2 > n) n else fib(n-1) + fib(n-2)
   * fib(12)
   */

  val testProg = Prog(mutable.HashMap("fib" -> FDecl("fib", List(Var("n")),
    IfStm(
      BExp.BinExp(Integer(2), Var("n"), GtOp()),
      ExpStm(Var("n")),
      ExpStm(
        AExp.BinExp(
          CallExp("fib", List(AExp.BinExp(Var("n"), Integer(1), Sub()))),
          CallExp("fib", List(AExp.BinExp(Var("n"), Integer(2), Sub()))),
          Plus()
        )
      )))),
    ExpStm(CallExp("fib", List(Integer(12)))))


  /*
   * Test to ensure that locally defined variables does not escape function scope
   */

  val testProg1 = Prog(mutable.HashMap("test" -> FDecl("test", List(Var("a")),
    ExpStm(AExp.BinExp(Var("a"), Integer(2), Mul())))),
    CompStm(
      ExpStm(CallExp("test", List(Integer(2)))),
      ExpStm(Var("a"))
    )
  )

  /*
   * fun sum(a, b, c) = {
   *  var x = a + b
   *  var y = b + c
   *  var z = x + y - b
   * }
   */
  val testSymProg = Prog(mutable.HashMap("sum" -> FDecl("sum", List(Var("a"), Var("b"), Var("c")),
    CompStm(
      AssignStm(
        Var("x"),
        AExp.BinExp(Var("a"), Var("b"), Plus())
      ),
      CompStm(
        AssignStm(
          Var("y"),
          AExp.BinExp(Var("b"), Var("c"), Plus())
        ),
        AssignStm(
          Var("z"),
          AExp.BinExp(
            AExp.BinExp(
              Var("x"),
              Var("y"),
              Plus()
            ),
            Var("b"),
            Sub()
          )
        )
      )
    )
    )),
    ExpStm(CallExp("sum", List(Sym("a"), Sym("b"), Sym("c"))))
  )
  val resConcrete = concreteInterp.interpProg(testProg)
  val resSymbolic = symbolicInterp.interpProg(testSymProg)
  println(s"result was: ${symbolicInterp.symExprToString(resSymbolic.e)}")
}
