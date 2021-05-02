package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // EXERCISE3.25
  // 2分木ノード(Leaf と Branch)の数を数える size 関数を記述せよ。
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // EXERCISE3.26
  // Tree[Int]の最大の要素を返す maximum関数 を記述せよ。
  // なおScalaでは、x.max(y) または x max y を使って2つの整数xとyの最大値を計算できる。
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(v)      => v
    case Branch(l, r) => max(l).max(max(r))
  }

  // EXERCISE3.27
  // 2分木のルートから任意のLeafまでの最長パスを返す depth関数 を記述せよ。
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  // EXERCISE3.28
  // 2分木の各要素を特定の関数を使って変更する map関数 を記述せよ。
  // この関数は List の同じ名前のメソッドに類似している。
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

}
