package state

import State._

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // EXERCISE6.1
  // RNG.nextInt を使って 0 ~ Int.maxValue(0 と Int.maxValueを含む)のランダムな整数を生成する関数を記述せよ。
  // なお、nextInt が Int.MinValue を返すときには、対応する自然数がない。この得意なケースにも対処する必要がある。
  // memo: Int.MinValue = -2147483648, Int.MaxValue = 2147483647
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i < 0) (-(i + 1), r) else (i, r)
  }

  // EXERCISE6.2
  // 0 ~ 1(1を含まない)のDouble型の値を生成する関数を記述せよ。
  // Int.MaxValueを使って値の整数の最大値を取得できることと、x.toDoubleを使って x: Int を Double に変換できることに注意
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / Int.MaxValue.toDouble + 1, r)
  }

  // EXERCISE6.3
  // ペア(Int, Double)、ペア(Double, Int)、および3要素のタプル(Double, Double, Double)を生成する関数を記述せよ。
  // すでに作成済みの関数を再利用できるはずだ。
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // EXERCISE6.4
  // ランダムな整数のリストを生成する関数を記述せよ。
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      count match {
        case n if n > 0 => {
          val (i, r) = rng.nextInt
          go(count - 1, r, i :: acc)
        }
        case _ => (acc.reverse, rng)
      }
    }
    go(count, rng, Nil)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (List(), rng)
    } else {
      val (i, r) = rng.nextInt
      val (l, r2) = ints(count - 1)(r)
      (i :: l, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, r2) = s(rng)
      (f(a), r2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // EXERCISE6.5
  // map を使って double をもう少し要領よく実装し直せ。EXERCISE6.2を参照。
  def doubleUseMap: Rand[Double] =
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)

  // EXERCISE6.6
  // 以下のシグネチャに基づいて map2 を実装せよ。この関数は、 ra と rb の2つのアクションと、
  // それらの結果を結合する 関数f を受け取り、それらを結合する新しいアクションを返す。
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  // EXERCISE6.7
  // [難問] 2つのRNG遷移の組み合わせが可能であるとしたら、それらのリスト全体を結合することも可能であるはずだ。
  // 遷移のListを1つの遷移にまとめるための sequence を実装せよ。それを使って、以前に記述した ints関数 を再実装せよ。
  // その際には、標準ライブラリのList.fill(n)(x)関数 を使って x を n 回繰り返すリストを作成できる。
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsUseSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def nonNegativeLessThan_(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // EXERCISE6.8
  // flatMap を実装し、それを使って nonNegativeLessThan 実装せよ。
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  // EXERCISE6.9
  // flatMap を使って map と map2 を再実装せよ。
  // これが可能であることは、flatMap がmap と map2 よりも強力であると述べていることから明らかである。
  def mapUseFlatMap[A, B](ar: Rand[A])(f: A => B): Rand[B] =
    flatMap(ar)(a => unit(f(a)))

  def map2UseFlatMap[A, B, C](ar: Rand[A],
                              br: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ar) { a =>
      map(br)(b => f(a, b))
    }

  def rollDie: Rand[Int] = mapUseFlatMap(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = s
      f(a).run(s1)
    })
}

object State {
  type Rand[A] = State[RNG, A]

  // EXERCISE6.10
  // unit, map, map2, flatMap, sequence の5つの関数を一般化せよ。
  // 可能であれば、それらをStateケースクラスのメソッドとして追加せよ。それが不可能であれば、State コンパニオンオブジェクトに配置せよ。
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update =
    (i: Input) =>
      (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _))        => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _))  => s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
}
