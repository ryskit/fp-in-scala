package parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  // 定数値を並列計算に昇格させる
  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // 2つの並列計算の結果を2項関数で結合
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  // 計算を並列評価の対象としてマークする。この評価はrunによって強制されるまで実際には発生しない。
  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })

  // 評価されていない引数をParでラッピングし、並列評価の対象としてマークする
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 与えられたParを完全に評価し、forkによって要求される並列計算を生成し、結果の値を取得
  // 実際に計算を行うことで、Parから値を取得する
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // EXERCISE7.4
  // このAPIでは、すでにさまざまな演算が可能である。簡単な例として、lazyUnit を使って関数を記述せよ。
  // この関数は、任意の関数 A => Bから、その結果を非同期で評価する関数へと変換する。
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // EXERCISE7.5
  // [難問] このsequence という関数を記述せよ。追加のプリミティブは必要ない。runを呼び出さないこと。
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // EXERCISE7.6
  // リストの要素を並行してフィルタリングする parFilter を実装せよ。
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](es: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(es).get == p2(es).get

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  // EXERCISE7.1
  // Par.map2 は2つの並列計算の結果を結合する新しい高階関数である。このシグネチャはどのようなものになるか。
  // Intにのみ対応すると想定せず、できるだけ汎用的なシグネチャを示せ。
  //
  // A.
  // def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = ???

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  // EXERCISE7.11
  // choiceNを実装し、choiceNをベースにchoiceを実装せよ
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val idx = run(es)(n).get
      run(es)(choices(idx))
    }

  def choiceUseChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(c => if (c) 0 else 1))(List(t, f))

  // EXERCISE7.12
  // choiceN にもやや不自然なところがある。Listからの選択が具体的すぎるように思えることだ。
  // なぜコンテナの種類が重要なのか。たとえば、計算のリストの代わりにそれらのMapを使った場合はどうなるか。
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  // EXERCISE7.13
  // この新しいプリミティブ chooser を実装し、それを使って choice と choiceNを実装せよ。
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a: A = run(es)(pa).get
      run(es)(choices(a))
    }

  def choiceUseChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(result => if (result) t else f)

  def choiceNUseChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(idx => choices(idx))
}
