package questions.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
//  def size[A](tree: Tree[A]): Int = ??

//  def maximum(tree: Tree[Int]): Int = ??

//  def depth[A](tree: Tree[A]): Int = ??

//  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = ??

//  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = ??

//  def size2[A](tree: Tree[A]): Int = ??

//  def maximum2(tree: Tree[Int]): Int = ??

//  def depth2[A](tree: Tree[A]): Int = ??

//  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = ??

  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(3), Leaf(1)), Branch(Leaf(5), Leaf(9)))
    println(t)
  }
}