package answers.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => depth(l) max depth(r)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => l + r + 1)

  def maximum2(tree: Tree[Int]): Int = fold(tree)(v => v)((l, r) => l max r)

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => l max r)

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B]
    = fold(tree)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))

  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(3), Leaf(1)), Branch(Leaf(5), Leaf(9)))
    println(t)
    println(map(t)(x => 2 * x))
    println(map2(t)(x => 2 * x))
  }
}