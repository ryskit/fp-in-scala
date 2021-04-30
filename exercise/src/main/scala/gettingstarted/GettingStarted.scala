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
}
