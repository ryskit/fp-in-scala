package errorhandling

sealed trait Option[+A] {
  // EXERCISE4.1
  // リスト4-4 のすべての関数をOptionで実装せよ。
  // 各関数を実装するときに、その関数の意味とそれを使用するであろう状況について考えること。
  // どの関数をいつ使用するかについては、後ほど考察する。
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _               => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    } catch { case e: Exception => 43 }
  }

//  def mean(xs: Seq[Double]): Double =
//    if (xs.isEmpty) throw new ArithmeticException("mean of empty list!")
//    else xs.sum / xs.length

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // EXERCISE4.2
  // flatMap をベースとして variance 関数を実装せよ。シーケンスの平均をm、シーケンスの各要素をxとすれば、
  // 分散は math.pow(x - m, 2) の平均となる
  def variance(s: Seq[Double]): Option[Double] =
    mean(s).flatMap(m => mean(s.map(x => math.pow(x - m, 2))))

  // EXERCISE4.3
  // 2項関数を使って Option型 の2つの値を結合する 総称関数map2 を記述せよ。
  // どちらかのOption値がNoneの場合は、戻り地もNoneになる。シグネチャは以下のとおり。
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(_a => b.map(_b => f(_a, _b)))

  // [別解]
  //  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  //    for {
  //      _a <- a
  //      _b <- b
  //    } yield f(_a, _b)

  // [別解]
  //  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  //    (a, b) match {
  //      case (None, _)          => None
  //      case (_, None)          => None
  //      case (Some(a), Some(b)) => Some(f(a, b))
  //    }

  // EXERCISE4.4
  // Optionのリストを1つのOptionにまとめる sequence関数 を記述せよ。
  // 新しいOptionには、元のリストに含まれているすべてのSome値のリストが含まれる。元のリストにNoneが１つでも含まれていた場合、
  // この関数の結果はNoneになる。それ以外の場合は、すべての値のリストを含んだSomeになる。シグネチャは以下のとおり。
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => Some(Nil)
    case h :: t => h.flatMap(_h => sequence(t) map (_h :: _))
  }

  // EXERCISE4.5
  // この traverse関数 を実装せよ。map と sequence を使用すればかんたんだが、リストを1回だけ調べる、
  // より効率のよい実装にすること。要するに、 traverse の観点から sequence を実装すればよい。
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil    => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

}
