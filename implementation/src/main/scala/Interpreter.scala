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

  case class Integer(x: Int) extends Expression

  case class Variable(name: String) extends Expression

  case class PlusOp(e1: Expression, e2: Expression) extends Expression

  case class MinusOp(e1: Expression, e2: Expression) extends Expression

  case class MultOp(e1: Expression, e2: Expression) extends Expression

  case class DivOp(e1: Expression, e2: Expression) extends Expression

  case class GtOp(e1: Expression, e2: Expression) extends Expression

  case class EqOp(e1: Expression, e2: Expression) extends Expression


  sealed trait Statement

  case class ExpStm(e: Expression) extends Statement

  case class AssignStm(v: Variable, e: Expression) extends Statement

  case class SeqStm(s1: Statement, s2: Statement) extends Statement

  case class IfStm(cond: Expression, thenStm: Statement, elseStm: Statement) extends Statement

  case class WhileStm(cond: Expression, doStm: Statement) extends Statement

  case class PrintStm(e: Expression) extends Statement


  /** ***********************************************************************/

  /**
    * Entry method for interpreting a program. A program is simply a statement.
    *
    * @param stm statement representing the program
    * @return the result of running the program.
    */
  def interp(stm: Statement): HashMap[String, Int] = {

    /**
      * One of two workhorses in the interpretation. used to recursively interpret Statements. calls interpExp whenever
      * an expression must be interpreted.
      *
      * @param stm the statement to be interpreted
      * @param env the current version of the environment; A list entries which binds a value to a name.
      * @return the resulting value of the statement
      */
    def interpStm(stm: Statement, env: HashMap[String, Int]): HashMap[String, Int] =
      stm match {
        case ExpStm(e) =>
          interpExp(e, env)
          env
        case AssignStm(Variable(name), e) =>
          val v = interpExp(e, env)
          env updated (name, v)
        case SeqStm(s1, s2) =>
          interpStm(s2, interpStm(s1, env))
        case IfStm(cond, thenStm, elseStm) =>
          val v = interpExp(cond, env)
          if (v != 0) interpStm(thenStm, env) else interpStm(elseStm, env)
        case WhileStm(condExp, doStm) =>
          interpWhile(condExp, doStm, env)
        case PrintStm(e) =>
          println(interpExp(e, env))
          env
      }

    //TODO maybe find a better name.
    def interpWhile(condExp: Expression, doStm: Statement, env: HashMap[String, Int]): HashMap[String, Int] = {
      def doWork(env: HashMap[String, Int]): HashMap[String, Int] = {
        val cond = interpExp(condExp, env)
        if (cond != 0) {
          doWork(interpStm(doStm, env))
        } else {
          env
        }
      }

      doWork(env)
    }

    /**
      * Second workhorse in the interpretation. Used to recursively interpret an expression.
      *
      * @param e   The expression to be interpreted.
      * @param env The current version of the environment; A list of entries which binds a value to a name.
      * @return the value of the expression.
      */
    def interpExp(e: Expression, env: HashMap[String, Int]): Int =
      e match {
        case Integer(x) => x
        case Variable(name) => {
          val res = env get name
          res match {
            case Some(value) => value
            case None => throw new NoSuchElementException(s"can't find variable with name  $name")
          }
        }
        case PlusOp(e1, e2) => interpExp(e1, env) + interpExp(e2, env)
        case MinusOp(e1, e2) => interpExp(e1, env) - interpExp(e2, env)
        case MultOp(e1, e2) => interpExp(e1, env) * interpExp(e2, env)
        case DivOp(e1, e2) => interpExp(e1, env) / interpExp(e2, env)
        case GtOp(e1, e2) => if (interpExp(e1, env) > interpExp(e2, env)) 1 else 0
        case EqOp(e1, e2) => if (interpExp(e1, env) == interpExp(e2, env)) 1 else 0
      }

    val env: HashMap[String, Int] = new HashMap()
    interpStm(stm, env)
  }

  // 2 + 2
  val testProg1 = ExpStm(PlusOp(Integer(2), Integer(10)))

  // If 2 > 3 then 1 else 2
  val testProg2 = IfStm(GtOp(Integer(2), Integer(3)), ExpStm(Integer(1)), ExpStm(Integer(2)))

  // a = 11
  // a
  val testProg3 = SeqStm(AssignStm(Variable("a"), Integer(11)), ExpStm(Variable("a")))

  // b = 3
  // if b > 2 then b = 2 else b
  //print b
  val testProg4 = SeqStm(
    AssignStm(Variable("b"), Integer(3)),
    IfStm(GtOp(Variable("b"), Integer(2)),
      AssignStm(Variable("b"), Integer(2)),
      ExpStm(Variable("b"))))

  /*
    b = 3
    if b > 4 then b = 1 else b = 0
    print b
   */
  val testProg5 = SeqStm(
    SeqStm(
      AssignStm(Variable("b"), Integer(3)),
      IfStm(GtOp(Variable("b"), Integer(4)),
        AssignStm(Variable("b"), Integer(1)),
        AssignStm(Variable("b"), Integer(0)))
    ),
    PrintStm(Variable("b"))
  )
  /*
   * a = 1
   * i = 0
   * while 10 > i do
   *   a = a * 2
   *   i = i + 1
   * print a
   */
  val testProg6 = SeqStm(
    SeqStm(
      AssignStm(Variable("a"), Integer(1)),
      AssignStm(Variable("i"), Integer(0))
    ),
    SeqStm(
      WhileStm(GtOp(Integer(10), Variable("i")),
        SeqStm(AssignStm(Variable("a"), MultOp(Variable("a"), Integer(2))),
          AssignStm(Variable("i"), PlusOp(Variable("i"), Integer(1))))),
      PrintStm(Variable("a"))
    )
  )

  interp(testProg6)

}


