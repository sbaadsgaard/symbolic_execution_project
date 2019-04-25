package result
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

case class Ok[V](v: V) extends Result[V, Nothing]
case class Error[E](e: E) extends Result[Nothing, E]

object Result {
  def sequence[V, E](vs: List[Result[V,E]]): Result[List[V], E] = vs match {
    case Nil => Ok(Nil)
    case hd::tl => hd.flatMap( v => sequence(tl).map(v :: _))
  }

  def traverse[V, W, E](vs: List[V])(f: V => Result[W, E]): Result[List[W], E] = vs match {
    case Nil => Ok(Nil)
    case hd::tl => f(hd).flatMap( w => traverse(tl)(f).map(w :: _))
  }
}
