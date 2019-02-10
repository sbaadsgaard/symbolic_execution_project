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

  case class FDecl(id: Id, args: List[Id], fbody: Statement)

  /*
   * A program consist of zero or more top level function declarations, followed by 1 or more statements starting with
   * @stm
   */
  case class Prog(funcs: HashMap[Id, FDecl], stm: Statement)


  def interpProg(p: Prog): Expression = {

    /*
      We interpret a statement in the current environment @venv and return a possibly updated environment
     */
    def interpStm(statement: Interpreter.Statement, venv: HashMap[Id, Int]): (Expression, HashMap[Id, Int]) = {
      statement match {
        case ExpStm(e) => (interpExp(e, venv), venv)
        case AssignStm(id, valExp) =>
          val v = interpExp(valExp, venv)
          v match {
            case Integer(i) => (Integer(i), venv.updated(id, i))
            case _ => throw new IllegalArgumentException("Can only assign Integers to variables")
          }
        case CompStm(s1, s2) =>
          val (_, newEnv) = interpStm(s1, venv)
          interpStm(s2, newEnv)
        case IfStm(cond, thenStm, elseStm) =>
          val c = interpExp(cond, venv)
          c match {
            case Bool(v) => if (v) interpStm(thenStm, venv) else interpStm(elseStm, venv)
            case _ => throw new IllegalArgumentException("malformed conditional expression")
          }
        case WhileStm(cond, stm) => {
          interpWhile(cond, stm, venv)
        }
      }
    }

    def interpWhile(cond: BExpression, stm: Statement, venv: HashMap[Id, Int]): (Expression, HashMap[Id, Int]) = {

      def dowork(venv: HashMap[Id, Int]): HashMap[Id, Int] = {
        val v = interpBexp(cond, venv)
        v match {
          case Bool(b) =>
            if (b) {
              val (_, newEnv) = interpStm(stm, venv)
              dowork(newEnv)
            }
            else venv
          case _ => throw new IllegalArgumentException("malformed conditional expression")
        }
      }

      (Nil(), dowork(venv))
    }

    def interpExp(e: Expression, venv: HashMap[Id, Int]): Expression = {
      e match {
        case bExp: BExpression => interpBexp(bExp, venv)
        case aExp: AExpression => interpAexp(aExp, venv)
        case Nil() => Nil()
      }
    }

    /*
     * We interpret call by first evaluation the expressions given as argument values, and then we add the function
     * arguments to a new local environment, which is passed onto the interpretation of the function body.
     */
    def interpCall(e: CExpression, venv: HashMap[Id, Int]): Expression = {
      val decl = p.funcs.get(e.id)
      decl match {
        case None => throw new NoSuchElementException(s"function ${e.id.name} not defined")
        case Some(f) =>
          val vals = e.argVals.map((exp: AExpression) => {
            val v = interpAexp(exp, venv)
            v match {
              case Integer(x) => x
              case _ => throw new IllegalArgumentException("functions can only take integer arguments")
            }
          })
          val argValPairs = f.args zip vals
          val addTo = (m: HashMap[Id, Int], t: (Id, Int)) => m.updated(t._1, t._2)
          val newEnv = argValPairs.foldLeft(venv)(addTo)
          val (res, _) = interpStm(f.fbody, newEnv)
          res
      }
    }

    def interpBexp(e: BExpression, venv: HashMap[Id, Int]): Expression = {
      e match {
        case b: Bool => b
        case bin: BinBExp => interpBinBExp(bin, venv)
      }
    }

    def interpAexp(e: AExpression, venv: HashMap[Id, Int]): Expression = {
      e match {
        case i: Integer => i
        case Id(name) =>
          val v = venv.get(Id(name))
          v match {
            case None => throw new NoSuchElementException(s"variable $name not defined")
            case Some(value) => Integer(value)
          }
        case bin: BinAExp => interpBinAExp(bin, venv)
        case cExp: CExpression => interpCall(cExp, venv)
      }
    }

    def interpBinBExp(e: BinBExp, venv: HashMap[Id, Int]): Expression = {
      val v1 = interpAexp(e.e1, venv)
      val v2 = interpAexp(e.e2, venv)
      (v1, v2) match {
        case (Integer(i), Integer(j)) =>
          e.op match {
            case GtOp() => Bool(i > j)
            case EqOp() => Bool(i == j)
          }
        case _ => throw new IllegalArgumentException("Cannot compare other types than integers")
      }
    }

    def interpBinAExp(e: BinAExp, venv: HashMap[Id, Int]): Expression = {
      val v1 = interpAexp(e.e1, venv)
      val v2 = interpAexp(e.e2, venv)
      (v1, v2) match {
        case (Integer(i), Integer(j)) =>
          e.op match {
            case Add() => Integer(i + j)
            case Sub() => Integer(i - j)
            case Mult() => Integer(i * j)
            case Div() => Integer(i / j)
          }
        case _ => throw new IllegalArgumentException("Cannot add other types than integers")
      }
    }

    val env: HashMap[Id, Int] = HashMap()
    val (v, _) = interpStm(p.stm, env)
    v
  }

  /*
   * Func to build a Prog from a statement. takes a stm, and returns a prog with a map from id main to func main with
   * the given stm as body
   */

  /*
   * fun fib(n) = if (2 > n) n else fib(n-1) + fib(n-2)
   * fib(12)
   */

  val testProg = Prog(
    HashMap(Id("fib") -> FDecl(Id("fib"), List(Id("n")),
      IfStm(
        BinBExp(Integer(2), Id("n"), GtOp()),
        ExpStm(Id("n")),
        ExpStm(
          BinAExp(
            CExpression(Id("fib"), List(BinAExp(Id("n"), Integer(1), Sub()))),
            CExpression(Id("fib"), List(BinAExp(Id("n"), Integer(2), Sub()))),
            Add()
          )
        )
      ))),
    ExpStm(
      CExpression(Id("fib"), List(Integer(12)))
    ))

  /*
   * Test to ensure that locally defined variables does not escape function scope
   */
  val testProg1 = Prog(HashMap(Id("test") -> FDecl(Id("test"), List(Id("a")),
    ExpStm(Id("a")))),
    CompStm(
      ExpStm(
        CExpression(Id("test"), List(Integer(42)))
      ),
      ExpStm(Id("a"))
    )
  )

  /*
   * Test to ensure that mutations of global variables only exists in the function scope
   */

  val testProg2 = Prog(HashMap(Id("test") -> FDecl(Id("test"), List.empty[Id],
    AssignStm(Id("x"), Integer(3)))),
    CompStm(
      AssignStm(Id("x"), Integer(1)),
      CompStm(
        ExpStm(CExpression(Id("test"), List.empty[AExpression])),
        ExpStm(Id("x"))
      )
    )
  )

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

  val testProg3 = Prog(HashMap(Id("pow") -> FDecl(Id("pow"), List(Id("a"), Id("b")),
    CompStm(
      AssignStm(Id("res"), Integer(1)),
      CompStm(
        AssignStm(Id("i"), Integer(0)),
        CompStm(
          WhileStm(
            BinBExp(Id("b"), Id("i"), GtOp()),
            CompStm(
              AssignStm(
                Id("res"),
                BinAExp(Id("res"), Id("a"), Mult())
              ),
              AssignStm(
                Id("i"),
                BinAExp(Id("i"), Integer(1), Add())
              )
            )
          ),
          ExpStm(Id("res"))
        )
      )
    )
  )),
    ExpStm(CExpression(Id("pow"), List(Integer(3), Integer(5))))
  )

  val res = interpProg(testProg3)
  println(s"result was: ${res.toString}")
}




