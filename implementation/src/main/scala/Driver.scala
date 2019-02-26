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

  println(System.getProperty("java.library.path"))

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

  /*
   * fun testme(x, y) {
   *  var z = 2*y
   *  if (z == x) {
   *    if (x > y + 10) {
   *      0
   *    } else {
   *      1
   *    }
   *  } else {
   *    2
   *  }
   * }
   *
   */
  val testSymProg1 = Prog(
    mutable.HashMap("testme" -> FDecl("testme", List(Var("x"), Var("y")),
      CompStm(
        AssignStm(Var("z"), AExp.BinExp(Integer(2), Var("y"), Mul())),
        IfStm(
          BExp.BinExp(
            Var("z"),
            Var("x"),
            EqOp()
          ),
          IfStm(
            BExp.BinExp(
              Var("x"),
              AExp.BinExp(
                Var("y"),
                Integer(10),
                Plus()
              ),
              GtOp()
            ),
            ExpStm(Integer(0)),
            ExpStm(Integer(1))
          ),
          ExpStm(Integer(2))
        ))
    )),
    ExpStm(CallExp(
      "testme",
      List(Sym("x"), Sym("y"))
    ))
  )

  /*
   * fun test(x,y) = {
   *  var z = 2*y
   *  var res = 0
   *  if (z == 2*x) {
   *    if (x > y + 10) {
   *      res = 1
   *    } else {
   *      res = 2
   *    }
   *  } else {
   *    res = 3
   *  }
   *  res*2
   * }
   */
  val testSymProg2 = Prog(
    mutable.HashMap("test" -> FDecl("test", List(Var("x"), Var("y")),
      CompStm(
        AssignStm(Var("z"), AExp.BinExp(Integer(2), Var("y"), Mul())),
        CompStm(
          AssignStm(Var("res"), Integer(0)),
          CompStm(
            IfStm(BExp.BinExp(Var("z"), Var("x"), EqOp()),
              IfStm(BExp.BinExp(Var("x"), AExp.BinExp(Var("y"), Integer(10), Plus()), GtOp()),
                AssignStm(Var("res"), Integer(2)),
                AssignStm(Var("res"), Integer(4))
              ),
              AssignStm(Var("res"), Integer(6))
            ),
            ExpStm(AExp.BinExp(Var("res"), Integer(3), Mul()))
          )
        )
      )
    )),
    ExpStm(
      CallExp("test", List(Sym("x"), Sym("y")))
    )
  )

  val testSymProg3 = Prog(
    mutable.HashMap("test" -> FDecl("test", List(Var("x"), Var("y")),
      CompStm(
        AssignStm(Var("res"), Integer(0)),
        CompStm(
          IfStm(BExp.BinExp(Var("x"), Var("y"), GtOp()),
            AssignStm(Var("res"), Var("x")),
            AssignStm(Var("res"), Var("y"))
          ),
          ExpStm(AExp.BinExp(Var("res"), Integer(2), Mul()))
        )
      )
    )),
    ExpStm(CallExp("test", List(Sym("x"), Sym("y"))))
  )

  /*
   fun pow(a, b) = {
    var r = 1
    var i = 0
    while (b > i) {
      r = r*a
      i = i + 1
    }
    return r
   }
   */
  val testSymProg4 = Prog(
    mutable.HashMap("pow" -> FDecl("pow", List(Var("a"), Var("b")),
      CompStm(
        AssignStm(Var("r"), Integer(1)),
        CompStm(
          AssignStm(Var("i"), Integer(0)),
          CompStm(
            WhileStm(BExp.BinExp(Var("b"), Var("i"), GtOp()),
              CompStm(
                AssignStm(Var("r"), AExp.BinExp(Var("r"), Var("a"), Mul())),
                AssignStm(Var("i"), AExp.BinExp(Var("i"), Integer(1), Plus()))
              )
            ),
            ExpStm(Var("r"))
          )
        )
      )
    )),
    ExpStm(CallExp("pow", List(Sym("a"), Sym("b"))))
  )
  val resConcrete = concreteInterp.interpProg(testProg)
  val resSymbolic = symbolicInterp.interpProg(testSymProg2, maxBranches = 10, currBranches = 0)

}
