package errorhandling

sealed trait Either[+E, +A] {
  // EXERCISE4.6
  // Right値を操作するmap, flatMap, orElse, map2 を Either に追加せよ。
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(v)  => Left(v)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v)  => Left(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(_a => b.map(_b => f(_a, _b)))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // EXERCISE4.7
  // Either で sequence と traversal を実装せよ。
  // これらは、エラーが発生した場合に、最初に検出されたエラーを返すものとする
  def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] = l match {
    case Nil    => Right(Nil)
    case h :: t => h.flatMap(_h => sequence(t).map(_s => _h :: _s))
  }

  def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    l match {
      case Nil    => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

}
