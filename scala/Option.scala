sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = {
        this match {
            case None => None 
            case Some(x) => Some(f(x))
        }
    }

    def getOrElse[B >: A](default: => B): B = {
        this match {
            case None => default
            case Some(x) => x
        }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        map(f).getOrElse(None)
    }

    def flatMap_1[B](f: A => Option[B]): Option[B] = {
        this match {
            case None => None
            case Some(x) => f(x)
        }
    }

    def orElse_1[B >: A](ob: => Option[B]): Option[B] = {
        this match {
            case None => ob
            case _ => this
        }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        map(Some(_)).getOrElse(ob)
    }

    def filter_1(f: A => Boolean): Option[A] = {
        this match {
            case None => None
            case Some(x) => if (f(x)) Some(x) else None
        }
    }

    def filter(f: A => Boolean): Option[A] = {
        flatMap(a => if (f(a)) Some(a) else None)
    }
}
case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing]

def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}

def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(
        m => mean(
            xs.map(
                i => math.pow(i - m, 2)
            )
        )
    )
}
