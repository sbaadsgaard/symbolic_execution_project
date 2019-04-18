import Result.{Error, Ok}

trait  Result[+V, +E] {
  def map[T](f: V => T): Result[T, E] = this match {
    case Ok(v) => Ok(f(v))
    case Error(e) => Error(e)
  }

  def flatMap[EE >: E, T](f: V => Result[T, EE]): Result[T, EE] = this match {
    case Ok(v) => f(v)
    case Error(e) => Error(e)
  }
}
object Result {
  case class Ok[V](v: V) extends Result[V, Nothing]
  case class Error[E](e: E) extends Result[Nothing, E]
}
