
package questions.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

//  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = ??

//  def sum2(ints: List[Int]): Int = ??

//  def product2(ds: List[Double]): Double = ??

//  def tail[A](l: List[A]): List[A] = ??

//  def setHead[A](l: List[A], h: A): List[A] = ??

//  def drop[A](l: List[A], n: Int): List[A] = ??

//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ??

//  def init[A](l: List[A]): List[A] = ??

//  def length[A](l: List[A]): Int = ??

//  @annotation.tailrec
//  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = ??

//  def sum3(ns: List[Int]): Int = ??

//  def product3(ds: List[Double]): Double = ??

//  def length2[A](l: List[A]): Int = ??

//  def reverse[A](l: List[A]): List[A] = ??

//  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = ??

//  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = ??

//  def concat[A](l: List[List[A]]): List[A] = ??

//  def add1(ints: List[Int]): List[Int] = ??

//  def doubleToString(ds: List[Double]): List[String] = ??

//  def map[A, B](l: List[A])(f: A => B): List[B] = ??

//  def filter[A](as: List[A])(f: A => Boolean): List[A] = ??

//  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = ??

//  def foldRightViaFoldLeft_2[A, B](l: List[A], z: B)(f: (A, B) => B): B = ??

//  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = ??

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    val list2 = List(2, 4, 6, 8)
  }

  // ------ Required review -------

//  def foldRightViaFoldLeft
//  def foldLeftViaFoldRight
//  def concat
//  def appendViaFoldRight
//  def appendViaFoldLeft
}
