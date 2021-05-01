package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil              => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil              => 1.0
    case Cons(0.0, _)     => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil              => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // EXERCISE3.1
  // 以下のマッチ式はどのような結果になるか。
  // x => 3になる
  //  val x = List(1, 2, 3, 4, 5) match {
  //    case Cons(x, Cons(2, Cons(4, _)))          => x
  //    case Nil                                   => 42
  //    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  //    case _                                     => 101
  //  }

  // EXERCISE3.2
  // Listの最初の要素を削除する関数tailを実装せよ。この関数の実行時間が一定であることに注意。
  // ListがNilである場合、実装上の選択肢として他に何があるか。この質問については、次章で再び取り上げる
  //
  // Nilである場合にNil以外の選択肢としては、Either型でErrorをラップして返すとか？
  def tail[A](l: List[A]): List[A] = l match {
    case Nil           => Nil
    case Cons(_, tail) => tail
  }

  // EXERCISE3.3
  // EXERCISE3.2 と同じ考え方に基づいて、Listの最初の要素を別の値と置き換える setHead 関数を実装せよ。
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil           => Nil
    case Cons(_, tail) => Cons(x, tail)
  }

  // EXERCISE3.4
  // tailを一般化して、リストの先頭からn個の要素を削除する drop という関数に書き換えよ。
  // この関数の実行時間は削除する要素の数にのみ比例することに注意。List全体のコピーを作成する必要はない。
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil           => Nil
    case l if n < 1    => l
    case Cons(_, tail) => drop(tail, n - 1)
  }

  // EXERCISE3.5
  // 述語とマッチする場合に限り、Listからその要素までの要素を削除する dropWhile を実装せよ。
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil                         => Nil
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _                           => l
  }

  // EXERCISE3.6
  // すべてがこのようにうまくいくわけではない。Listの末尾を除くすべての要素で構成されたListを返す init 関数を実装せよ。
  // List(1, 2, 3, 4) が与えられた場合、init は List(1, 2, 3) を返す。
  // この関数を tail のように一定時間で実装できないのはなぜか。
  //
  // A. Listは一方向に連結したConsで構成されているため、
  // 末尾の判定を行うには末尾要素まで進める必要があるのでListの長さに比例して処理時間が掛かる
  def init[A](l: List[A]): List[A] = l match {
    case Nil              => Nil
    case Cons(_, Nil)     => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  // EXERCISE3.7
  // foldRight を使って実装された product は、0.0を検出した場合に、直ちに再帰を中止して0.0を返せるか。
  // その理由を説明せよ。大きなリストで foldRight を呼び出した場合に短絡の仕組みについて検討せよ。
  // この問題は奥が深いため、第5章で改めて取り上げる。
  //
  // A. 返せない。末尾から処理が展開されていくため。
  // 大きなリストで foldRight を呼び出した場合、コールスタックが溢れてStackOverflowエラーが発生する。

  // EXERCISE3.8
  // foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)) のように、Nil および Cons 自体を foldRight に渡した場合はどうなるか。
  // これが foldRight と List のデータコンストラクタとの関係について何を表していると思うか。
  //
  // A. List(1, 2, 3)で定義した同様の値が返却される。=> Cons(1,Cons(2,Cons(3,Nil)))

  // EXERCISE3.9
  // foldRight を使ってリストの長さを計算せよ。
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  // EXERCISE3.10
  // この foldRight の実装は末尾再帰ではなく、リストが大きい場合は StackOverflowError になってしまう。
  // これをスタックセーフでないという。そうした状況であると仮定し、前章で説明した手法を使って、
  // リスト再帰の総称関数 foldLeft を記述せよ。シグネチャは以下のとおり。
  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil              => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  // EXERCISE3.11
  // foldLeft を使って sum, product, およびリストの長さを計算する関数を記述せよ。
  def sumLF(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def productLF(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
  def lengthLF(l: List[Int]) = foldLeft(l, 0)((acc, _) => acc + 1)

  // EXERCISE3.12
  // 要素が逆に並んだリストを返す関数を記述せよ。List(1, 2, 3)が与えられた場合、この関数はList(3, 2, 1)を返す。
  // 畳み込みを使って記述できるかどうか確認すること。
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, head) => Cons(head, acc))

  // EXERCISE3.13
  // [難問] foldRight をベースとして foldLeft を記述することは可能か。その逆はどうか。
  // foldLeft を使って foldRight を実装すると、foldRight を末尾再帰的に実行することが可能となり、
  // 大きなリストでもスタックオーバーフローが発生しなくなるので便利である。

  // EXERCISE3.14
  // foldLeft または foldRight をベースとして append を実装せよ。

  // EXERCISE3.15
  // [難問] 複数のリストからなるリストを1つのリストとして連結する関数を記述せよ。
  // この関数の実行時間はすべてのリストの長さの合計に対して線形になるはずである。すでに定義した関数を使ってみること。
}
