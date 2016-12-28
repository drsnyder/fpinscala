package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def size[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }


  def maximum(t: Tree[Int]): Int = {
    def go(t: Tree[Int], m: Int): Int = t match {
      case Leaf(x) => x max m
      case Branch(l, r) => m max (go(l, m) max go(r, m))
    }
    go(t, 0)
  }

  def depth[A](t: Tree[A]): Int = {
    def go[A](t: Tree[A], depth: Int): Int = t match {
      case Leaf(x) => 0
      case Branch(l, r) => 1 + go(l, depth+1) max go(r, depth+1)
    }
    go(t, 0)
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }


  def fold[A,B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
    case Leaf(x) => f(x, z)
    case Branch(l, r) => (l,r) match {
      case (Leaf(vl), Leaf(vr)) => f(vl, f(vr, z))
      case (x, Leaf(vr)) => fold(x, f(vr, z))(f)
      case (Leaf(vl), x) => fold(x, f(vl, z))(f)
      case (x, y) => fold(x, fold(y, z)(f))(f)
    }
  }

  def maximumf(t: Tree[Int]): Int =
    fold(t, 0)((a, b) => a max b)

  //def depthf[A](t: Tree[A]): Int =
  //  fold(t, 0)((_, b) => t + b)


}