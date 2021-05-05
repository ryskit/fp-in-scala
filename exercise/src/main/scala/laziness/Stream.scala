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
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _          => acc
    }
    go(this, List()).reverse
  }

  // EXERCISE5.2
  // Streamの先頭から n個の要素を取り出す関数 take(n)関数 と、
  // Streamの先頭から n個の要素をスキップする drop(n)関数 を記述せよ。
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  @scala.annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  // EXERCISE5.3
  // Streamの先頭から指定された述語とマッチする要素をすべて取り出す takeWhile関数 を記述せよ。
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _                    => empty
  }

  def exists(f: A => Boolean): Boolean = this match {
    case Cons(h, t) => f(h()) || t().exists(f)
    case _          => false
  }

  def exists2(f: A => Boolean) =
    foldRight(false)((a, b) => f(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  // EXERCISE5.4
  // Stream の要素のうち、指定された述語とマッチするものをすべてチェックする forAll を実装せよ。
  // この実装では、マッチしない値が検出された時点でチェックを終了しなければならない。
  def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

//  def forAll(f: A => Boolean): Boolean = this match {
//    case Cons(h, t) => if (f(h())) t().forAll(f) else false
//    case _          => true
//  }
  // ((a, b) => if (f(h())) cons(h(), ) else empty)

  // EXERCISE5.5
  // foldRight を使って takeWhile を実装せよ。
  def takeWhileUseFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty)

  // EXERCISE5.6
  // [難問] foldRight をつかって headOption を実装せよ。
  def headOptionUseFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // EXERCISE5.7
  // foldRight をつかって map, filter, append, flatMap を実装せよ。
  // append メソッドはその引数に関して非正格でなければならない
  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def find(f: A => Boolean): Option[A] = filter(f).headOption
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

  // EXERCISE5.8
  // ones を少し一般化し、指定された値の無限ストリームを返す constant 関数を記述せよ。
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // EXERCISE5.9
  // n で始まって n + 1, n + 2 と続く整数の無限ストリームを生成する関数を記述せよ
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // EXERCISE5.10
  // フィボナッチ数列(0, 1, 1, 2, 3, 5, 8, ...)の無限ストリームを生成する fibs関数 を記述せよ。
  def fibs(): Stream[Int] = {
    def go(n: Int, m: Int): Stream[Int] =
      cons(n, go(m, n + m))
    go(0, 1)
  }

  // EXERCISE5.11
  // より汎用的なストリーム生成関数 unfold を記述せよ。この関数は、初期状態に加えて、
  // 以下の状態と、生成されるストリームの次の値を生成する関数を受け取る
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None         => empty
    }

  // EXERCISE5.12
  // unfold を使って fibs, from, constant, onesを記述せよ。
  def onesUseUnfold(): Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

  def constantUseUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def fromUseUnfold(n: Int): Stream[Int] = unfold(n)(_ => Some(n, n + 1))

  def fibsUseUnfold(): Stream[Int] =
    unfold((0, 1)) {
      case (s0, s1) => Some((s0, (s1, s0 + s1)))
    }
}
