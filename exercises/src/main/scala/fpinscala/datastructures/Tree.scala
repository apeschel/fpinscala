package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](n: Tree[A]): Int = n match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(n: Tree[Int]): Int = n match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](n: Tree[A]): Int = n match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](n: Tree[A])(f: A => B): Tree[B] = n match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](n: Tree[A])(f: A => B)(g: (B,B) => B): B = n match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](n: Tree[A]): Int =
    fold(n)(_ => 1)(1 + _ + _)

  def maximumViaFold(n: Tree[Int]): Int =
    fold(n)(x => x)(_ max _)

  def depthViaFold[A](n: Tree[A]): Int =
    fold(n)(_ => 0)((d1,d2) => 1 + (d1 max d2))

  def mapViaFold[A,B](n: Tree[A])(f: A => B): Tree[B] =
    fold(n)(v => Leaf(f(v)): Tree[B])(Branch(_,_))
}