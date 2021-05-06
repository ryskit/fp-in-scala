package state

import scala.annotation.tailrec

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
}
