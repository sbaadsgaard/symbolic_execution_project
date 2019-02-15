import Grammar._
import Aop._
import Bop._
import AExp._
import Grammar.Stm._
import Grammar.Prog
import Grammar.FDecl
import scala.collection.mutable

object Driver extends App {

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
   * Test to ensure that mutations of global variables only exists in the function scope
   */


  /*
   * fun pow(a, b) {
   *  res = 1
   *  i = 0
   *  while (b > i) do
   *    res = res*a
   *    i = 1 + 1
   *  res
   * }
   *
   * pow(2,3)
   */

  val res = Interpreter.interpProg(testProg)
  println(s"result was: ${res.toString}")
}
