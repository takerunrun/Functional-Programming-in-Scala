
package datastructures

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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)((x, y) => x + y)

  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)((x, y) => x * y)


  // ---------- Exercise ---------
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, x) => x + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)((x, y) => x + y)

  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)((x, y) => x * y)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((as, h) => Cons(h, as))

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((h, as) => Cons(h, as))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((as, h) => Cons(h, as))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def add1(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int])((h, t) => Cons(h+1, t))

  def doubleToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // ------ Answers ------
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    val list2 = List(2, 4, 6, 8)
    print(filter(list)(i => i % 2 == 0))
  }

  // ------ Required review -------

//  def foldRightViaFoldLeft
//  def foldLeftViaFoldRight
//  def concat
//  def appendViaFoldRight
//  def appendViaFoldLeft
}
