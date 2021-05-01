package gettingstarted

object GettingStarted {
  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(isSorted(Array(1, 2), (x: Int, y: Int) => x < y))
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int): Unit = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  // EXERCISE2.1
  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, prev: Int, current: Int): Int = {
      if (n <= 0) prev
      else go(n - 1, current, prev + current)
    }
    go(n, 0, 1)
  }

  // EXERCISE2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Seq[A]): Boolean = {
      n match {
        case Seq(x, y, tails @ _*) =>
          if (ordered(x, y)) loop(y +: tails)
          else false
        case _ => true

      }
    }
    loop(as.toSeq)
  }

  // EXERCISE2.3
  // カリー化(currying)では、引数2つの関数fがfを部分的に適用する引数１つの関数に変換される。
  // この場合も、コンパイルできる実装は1つだけである。この実装を記述せよ。
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  // EXERCISE2.4
  // curryにおる変換を逆向きに行うuncurryを実装せよ。=>は右結合であるため、A => (B => C) は、
  // A => B => C と記述できる。
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // EXERCISE2.5
  // 2つの関数を合成する高階関数を実装せよ。
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
