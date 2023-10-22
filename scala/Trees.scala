sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def size[A](t: Tree[A]): Int = {
        t match {
            case Branch(l, r) => size(l) + size(r)
            case Leaf(_) => 1
        }
    }

    def maximum(t: Tree[Int]): Int = {
        t match {
            case Branch(l, r) => maximum(l).max(maximum(r))
            case Leaf(x) => x
        }
    }

    def depth[A](t: Tree[A]): Int = {
        t match {
            case Branch(l, r) => 1 + depth(l) max depth(r)
            case Leaf(x) => 0
        }
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
        t match {
            case Branch(l, r) => Branch(map(l)(f), map(r)(f))
            case Leaf(x) => Leaf(f(x))
        }
    }

    def fold[A, B](t: Tree[A])(g: A => B)(f: (B, B) => B): B = {
        t match {
            case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
            case Leaf(x) => g(x)
        }
    }

    def sizeWithFold[A](t: Tree[A]): Int = fold(t)(a => 1)(_ + _)

    def maxWithFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

    def depthWithFold[A](t: Tree[A]): Int = fold(t)(a => 0)((x, y) => 1 +  (x max y))

    def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
}
