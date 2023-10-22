
sealed trait List[+A]
case object Nil extends List[Nothing] 
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def reduce[A](l: List[A], f: (A, A) => A, initialR: A): A = {
    @annotation.tailrec
    def loop(next: List[A], rSoFar: A): A = {
      next match {
        case Nil => rSoFar
        case Cons(h, t) => loop(t, f(rSoFar, h))
      }
    }

    loop(l, initialR)
  }


  def sum(ints: List[Int]): Int = {
    @annotation.tailrec
    def loop(rollingSum: Int, next: List[Int]): Int = {
      next match {
        case Nil => rollingSum
        case Cons(h, t) => loop(rollingSum + h, t)
      }
    }
    loop(0, ints)
  }

  def test(): Int = {
    List(1, 2, 3) match {
      case Nil => 42
      case _ => 100
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = {
    l match {
        case Nil => Nil
        case Cons(h, t) => t
    }
  }

  def setHead[A](l: List[A], nh: A): List[A] = {
    l match {
        case Nil => Nil
        case Cons(h, t) => Cons(nh, t)
    }
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
        case Nil => Nil
        case Cons(h, t) => if (n == 0) l else drop(t, n - 1)
    }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
        case Nil => Nil
        case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }
  }

  def foldRightShortCircuit[A, B](l: List[A], z: B)(f: (A, B) => B, g: A => Boolean): B = {
      l match {
          case Nil => z
          case Cons(h, t) => if (g(h)) z else f(h, foldRightShortCircuit(t, z)(f, g))
      }
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
      l match {
          case Nil => z
          case Cons(h, t) => f(h, foldRight(t, z)(f))
      }
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
      l match {
          case Nil => z
          case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }
  }

  def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)

  def prod2(l: List[Int]): Int = foldRightShortCircuit(l, 0)(_ * _, _ == 0)

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => 1 + y)

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def prod3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => x + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((x, y) => Cons(y, x))

  def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((x, y) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, Nil:List[A])((x, y) => List.append(x, y))

  def map[A, B](l: List[A], f: A => B): List[B] = foldRight(l, Nil:List[B])((a, l) => Cons(f(a), l))

  def add1(l: List[Int]): List[Int] = map(l, x => x + 1)

  def dToS(l: List[Double]): List[String] = map(l, x => x.toString)

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil:List[A])((x, y) => if (f(x)) y else Cons(x, y))

  def filterOddOnes(l: List[Int]): List[Int] = filter(l)(x => x % 2 == 1)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l, f))

  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
      flatMap(l)(x => if (f(x)) Nil else List(x))
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
      (l1, l2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
        case (x, Nil) => x
        case (Nil, x) => x
      }
  }

  def hasSubsequence[A](l1: List[A], l2: List[A]): Boolean = {
      def go(itr1: List[A], itr2: List[A]): Boolean = {
          (itr1, itr2) match {
              case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2 && go(t1, t2)) true else go(t1, itr2)
              case (Nil, Nil) => true
              case (_, Nil) => true
              case (Nil, _) => false
          }
      }

      go(l1, l2)
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (t == Nil) Nil else Cons(h, init(t))
    }
  }

}
