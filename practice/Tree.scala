
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(n) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(left, right) => 1 + depth(left) max depth(right)
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  //def apply[A](left: A, right: A): Tree[A] =
    //Tree(Leaf(left), Leaf(right))

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
  // def fold[A,B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def sizef[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((x, z) => x + z)

  def maximumf(t: Tree[Int]): Int =
    fold(t)(i => i)((x, z) => x max z)
 
  def depthf[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((x, z) => 1 + x max z)
 
  def mapf[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(l => Leaf(f(l)): Tree[B])((l, r) => Branch(l, r))
}
