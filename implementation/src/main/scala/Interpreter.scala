import java.util.NoSuchElementException

import scala.collection.immutable.HashMap

/**
  * Simple interpreter for the language SImPL. We simply descent the AST.
  */
object Interpreter extends App {

  /** ***********************************************************************/
  /**
    * We define the language here
    */
  //TODO perhaps try to split op binary operators.

  sealed trait Expression

  case class Nil() extends Expression

  sealed trait BExpression extends Expression //Boolean Expression

  case class Bool(v: Boolean) extends BExpression

  case class BinBExp(e1: AExpression, e2: AExpression, op: BinBop) extends BExpression

  sealed trait BinBop

  case class GtOp() extends BinBop

  case class EqOp() extends BinBop

  sealed trait AExpression extends Expression // Arithmetic Expression

  case class CExpression(id: Id, argVals: List[AExpression]) extends AExpression

  case class Integer(x: Int) extends AExpression

  case class Id(name: String) extends AExpression

  case class BinAExp(e1: AExpression, e2: AExpression, op: BinAop) extends AExpression

  sealed trait BinAop

  case class Add() extends BinAop

  case class Sub() extends BinAop

  case class Mult() extends BinAop

  case class Div() extends BinAop

  sealed trait Statement

  case class ExpStm(e: Expression) extends Statement

  case class AssignStm(id: Id, valExp: AExpression) extends Statement

  case class CompStm(s1: Statement, s2: Statement) extends Statement

  case class IfStm(cond: BExpression, thenStm: Statement, elseStm: Statement) extends Statement

  case class WhileStm(cond: BExpression, bodyStm: Statement) extends Statement

  case class FDecl(id: Id, args: List[Id], stm: Statement) extends Statement

  case class Prog(funcs: HashMap[Id, FDecl])

  def interpProg(p: Prog): Expression = {

    /*
     * To interpret a statement we need the statement itself, a environment describing variables, and a prog containing
     * the current function declarations.
     */
    def interpStm(statement: Interpreter.Statement, venv: HashMap[Id, Int], pr: Prog): (Expression, HashMap[Id, Int], Prog) = {
      statement match {
        case ExpStm(e) => interpExp(e, venv, pr)
        case AssignStm(id, valExp) =>
          val (v, _, _) = interpExp(valExp, venv, pr)
          v match {
            case Integer(i) => (Integer(i), venv.updated(id, i), pr)
            case _ => throw new IllegalArgumentException("Can only assign Integers to variables")
          }
        case CompStm(s1, s2) =>
          val (_, newEnv, newProg) = interpStm(s1, venv, pr)
          interpStm(s2, newEnv, newProg)
        case IfStm(cond, thenStm, elseStm) =>
          val (c, _, _) = interpExp(cond, venv, pr)
          c match {
            case Bool(v) => if (v) interpStm(thenStm, venv, pr) else interpStm(elseStm, venv, pr)
            case _ => throw new IllegalArgumentException("malformed conditional expression")
          }
        case decl: FDecl =>
          val newProg: Prog = Prog(p.funcs.updated(decl.id, decl))
          (Nil(), venv, newProg)

      }
    }

    def interpExp(e: Expression, venv: HashMap[Id, Int], pr: Prog): (Expression, HashMap[Id, Int], Prog) = {
      e match {
        case bExp: BExpression => interpBexp(bExp, venv, pr)
        case aExp: AExpression => interpAexp(aExp, venv, pr)
        case Nil() => (Nil(), venv, pr)
      }
    }
    /*
     * We interpret call by first evaluation the expressions given as argument values, and then we add the function
     * arguments to a new local environment, which is passed onto the interpretation of the function body.
     * We return the old environment which does not contain the function arguments, nor any side effects from the body.
     * Thus function calls does not have side effects.
     */
    def interpCall(e: CExpression, venv: HashMap[Id, Int], pr: Prog): (Expression, HashMap[Id, Int], Prog) = {
      val decl = pr.funcs.get(e.id)
      decl match {
        case None => throw new NoSuchElementException(s"function ${e.id.name} not defined")
        case Some(f) =>
          val vals = e.argVals.map((exp: AExpression) => {
            val (v, _, _) = interpAexp(exp, venv, pr)
            v match {
              case Integer(x) => x
              case _ => throw new IllegalArgumentException("functions can only take integer arguments")
            }
          })
          val argValPairs = f.args zip vals
          val addTo = (m: HashMap[Id, Int], t: (Id, Int)) => m.updated(t._1, t._2)
          val newEnv = argValPairs.foldLeft(venv)(addTo)
          val (res, _, _) =  interpStm(f.stm, newEnv, pr)
          (res, venv, pr)
      }
    }

    def interpBexp(e: BExpression, venv: HashMap[Id, Int], pr: Prog): (Expression, HashMap[Id, Int], Prog) = {
      e match {
        case b: Bool => (b, venv, pr)
        case bin: BinBExp => interpBinBExp(bin, venv, pr)
      }
    }

    def interpAexp(e: AExpression, venv: HashMap[Id, Int], pr: Prog): (Expression, HashMap[Id, Int], Prog) = {
      e match {
        case i: Integer => (i, venv, pr)
        case Id(name) =>
          val v = venv.get(Id(name))
          v match {
            case None => throw new NoSuchElementException(s"variable $name not defined")
            case Some(value) => (Integer(value), venv, pr)
          }
        case bin: BinAExp => interpBinAExp(bin, venv, pr)
        case cExp: CExpression => interpCall(cExp, venv, pr)
      }
    }

    def interpBinBExp(e: BinBExp, venv: HashMap[Id, Int], pr: Prog): (Expression, HashMap[Id, Int], Prog) = {
      val (v1, _, _) = interpAexp(e.e1, venv, pr)
      val (v2, _, _) = interpAexp(e.e2, venv, pr)
      (v1, v2) match {
        case (Integer(i), Integer(j)) =>
          e.op match {
            case GtOp() => (Bool(i > j), venv, pr)
            case EqOp() => (Bool(i == j), venv, pr)
          }
        case _ => throw new IllegalArgumentException("Cannot compare other types than integers")
      }
    }

    def interpBinAExp(e: BinAExp, venv: HashMap[Id, Int], pr: Prog): (Expression, HashMap[Id, Int], Prog) = {
      val (v1, _, _) = interpAexp(e.e1, venv, pr)
      val (v2, _, _) = interpAexp(e.e2, venv, pr)
      (v1, v2) match {
        case (Integer(i), Integer(j)) =>
          e.op match {
            case Add() => (Integer(i + j), venv, pr)
            case Sub() => (Integer(i - j), venv, pr)
            case Mult() => (Integer(i * j), venv, pr)
            case Div() => (Integer(i / j), venv, pr)
          }
        case _ => throw new IllegalArgumentException("Cannot add other types than integers")
      }
    }

    /**
      * A program is a collection of functions, where main() is a function with no arguments, which will be the starting
      * point of the execution.
      */
    val mainFunc = p.funcs.get(Id("main"))
    mainFunc match {
      case None => throw new NoSuchElementException("Malformed program, missing main")
      case Some(f) =>
        val venv: HashMap[Id, Int] = new HashMap()
        val (e, _, _) = interpStm(f.stm, venv, p)
        e

    }
  }

  /*
   * Func to build a Prog from a statement. takes a stm, and returns a prog with a map from id main to func main with
   * the given stm as body
   */
  def buildProg(stm: Statement): Prog = {
    val m = HashMap(Id("main") -> FDecl(Id("main"), List.empty[Id], stm))
    Prog(m)
  }

  val testProg = Prog(
    HashMap(Id("main") -> FDecl(Id("main"),
      List.empty[Id], IfStm(
        BinBExp(Integer(511), Integer(8), GtOp()),
        ExpStm(Integer(1)),
        ExpStm(Integer(55))
      ))))

  /*
   * fun fib(n) = if (2 > n) n else fib(n-1) + fib(n-2)
   * fib(12)
   */
  val testProg1 = Prog(HashMap(Id("main") -> FDecl(Id("main"), List.empty[Id], CompStm(
    FDecl(Id("fib"),
      List(Id("n")),
      IfStm(BinBExp(Integer(2), Id("n"), GtOp()), ExpStm(Id("n")), ExpStm(
        BinAExp(
          CExpression(Id("fib"), List(BinAExp(Id("n"), Integer(1), Sub()))),
          CExpression(Id("fib"), List(BinAExp(Id("n"), Integer(2), Sub()))),
          Add()
        )))),
    ExpStm(CExpression(Id("fib"), List(Integer(12))))
  ))))

  /* Progam used to test that the function argument a does not escape the function scope.
   * fun test(a) = 2*a
   * test(a)
   * a
   *
   * gives runtime error "a not found" as expected
   */
  val testProg2 = buildProg(
    CompStm(
      FDecl(Id("test"), List(Id("a")), ExpStm(
        BinAExp(Integer(2), Id("a"), Mult()))),
      CompStm(
        ExpStm(CExpression(Id("test"), List(Integer(2)))),
        ExpStm(Id("a"))
      )
    )
  )

  /*
   * program used to test whether functions have side effect or not
   * a = 6
   * fun test() = {a = 3}
   * test()
   * a
   *
   * returns 6 as expected
   */
  val testProg3 = buildProg(
    CompStm(
      AssignStm(Id("a"), Integer(6)),
      CompStm(
        FDecl(Id("test"), List.empty[Id], AssignStm(Id("a"), Integer(3))),
        CompStm(
          ExpStm(CExpression(Id("test"), List.empty[AExpression])),
          ExpStm(Id("a"))
        )
      )
    )
  )

  val res = interpProg(testProg3)
  println(s"result was: ${res.toString}")
}




