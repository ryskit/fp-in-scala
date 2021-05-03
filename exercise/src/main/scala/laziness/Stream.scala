package laziness

import Stream._

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  // EXERCISE5.1
  // Stream を List に変換し、それによりストリームを強制的に評価する関数を記述せよ。
  // 結果は REPL で確認できる。標準ライブラリの通常の List型 への変換が可能である。
  // この関数と Stream を操作する他の関数は、Streamトレイトの中に配置できる。
  def toList: List[A] = {
    @scala.annotation.tailrec
    def loop(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Empty      => Nil
      case Cons(h, t) => loop(t(), h() :: acc)
    }
    loop(this, List())
  }

  // EXERCISE5.2
  // Streamの先頭から n個の要素を取り出す関数 take(n)関数 と、
  // Streamの先頭から n個の要素をスキップする drop(n)関数 を記述せよ。
  def take[A](n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  @scala.annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
