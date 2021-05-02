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
  def lengthLF[A](l: List[A]) = foldLeft(l, 0)((acc, _) => acc + 1)

  // EXERCISE3.12
  // 要素が逆に並んだリストを返す関数を記述せよ。List(1, 2, 3)が与えられた場合、この関数はList(3, 2, 1)を返す。
  // 畳み込みを使って記述できるかどうか確認すること。
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, head) => Cons(head, acc))

  // EXERCISE3.13
  // [難問] foldRight をベースとして foldLeft を記述することは可能か。その逆はどうか。
  // foldLeft を使って foldRight を実装すると、foldRight を末尾再帰的に実行することが可能となり、
  // 大きなリストでもスタックオーバーフローが発生しなくなるので便利である。
  def foldRightUseFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  // EXERCISE3.14
  // foldLeft または foldRight をベースとして append を実装せよ。
  def appendLF[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l1), l2)((acc, l) => Cons(l, acc))

  def appendRF[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((l, acc) => Cons(l, acc))

  // EXERCISE3.15
  // [難問] 複数のリストからなるリストを1つのリストとして連結する関数を記述せよ。
  // この関数の実行時間はすべてのリストの長さの合計に対して線形になるはずである。すでに定義した関数を使ってみること。
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  // EXERCISE3.16
  // 各要素に1を足すことで整数のリストを変換する関数を記述せよ。
  // 注意: これは新しいListを返す純粋関数になるはずである。
  def add1(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((a, acc) => Cons(a + 1, acc))

  // EXERCISE3.17
  // List[Double]の各値をStringに変換する関数を記述せよ。
  // d.toString という式を使って d: Double を String に変換できる
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((head, acc) => Cons(head.toString, acc))

  // EXERCISE3.18
  // リストの各要素を変更し、かつリストの構造をそのまま保つ総称関数mapを記述せよ。
  // この関数のシグネチャは以下の通り。
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((head, acc) => Cons(f(head), acc))

  def mapUseLeftFold[A, B](l: List[A])(f: A => B): List[B] =
    foldRightUseFoldLeft(l, List[B]())((head, acc) => Cons(f(head), acc))

  // EXERCISE3.19
  // 与えられた述語条件がみたされるまでリストから要素を削除する filter関数 を記述せよ。
  // この関数を使って List[Int] から奇数をすべて削除せよ。
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightUseFoldLeft(l, List[A]())(
      (head, acc) => if (f(head)) Cons(head, acc) else acc
    )

  // EXERCISE3.20
  // map と同じような働きをする flatMap関数 を記述せよ。この関数は単一の結果ではなくリストを返し、
  // そのリストは最終的な結果のリストに挿入されなければならない。この関数のシグネチャは以下の通り。
  // たとえば、 flatMap(List(1, 2, 3))(i => List(i, i)) は List(1,1,2,2,3,3) になるはずである。
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRightUseFoldLeft(l, List[B]())((head, acc) => appendLF(f(head), acc))

  // EXERCISE3.21
  // flatMap を使って filter を実装せよ。
  def filterUseFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) Cons(a, Nil) else Nil)

  // EXERCISE3.22
  // リストを2つ受け取り、対応する要素同士を足し合わせて新しいリストを生成する関数を記述せよ。
  // たとえば、List(1, 2, 3) と List(4, 5, 6) は List(5, 7, 9) になる。
  def sumWithPair(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(l1Head, l1Tail), Cons(l2Head, l2Tail)) =>
      Cons(l1Head + l2Head, sumWithPair(l1Tail, l2Tail))
  }

  // EXERCISE3.23
  // EXERCISE3.22 で作成した関数を、整数または加算に限定されなように一般化せよ。
  // 一般化された関数には zipWith という名前を付けること。
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(l1Head, l1Tail), Cons(l2Head, l2Tail)) =>
        Cons(f(l1Head, l2Head), zipWith(l1Tail, l2Tail)(f))
    }

  // EXERCISE3.24
  // [難問] 例として、Listに別のListがサブシーケンスとして含まれているかどうかを調べる hasSubsequence を実装せよ。
  // たとえば、List(1, 2, 3, 4) には、List(1,2)、List(2,3)、List(4) などがサブシーケンスとして含まれている。
  // 純粋関数型で、コンパクトで、かつ効率的な実装を見つけ出すのは難しいかもしれない。その場合は、それでかまわない。
  // どのようなものであれ、最も自然な関数を実装すること。その実装については、第5章で改めて取り上げ、改良する予定である。
  // なおScalaでは、任意の値x および yに対し、x == y という式を使って等しいかどうかを比較できる。
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil)                              => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _                                     => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t)                => hasSubsequence(t, sub)
  }

}
