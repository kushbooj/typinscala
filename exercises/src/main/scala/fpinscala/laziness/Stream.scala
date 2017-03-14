package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => Cons(h, () => t().take(n-1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, _) if (n == 0) => this
    case Cons(_, t) if(n > 0) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) => if(p(a)) Cons(() => a, () => b) else Empty )
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if(p(h())) => t().forAll(p)
    case _ => false
  }

  def forAll2(p: A => Boolean): Boolean =  {
    foldRight(true)((a, b) => p(a) && b)
  }

  def headOption2: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Option(h())
  }

//  def headOption: Option[A] = {
//    foldRight(None)((a,b) => Some(a))
//  }
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) =>  this
    case _ => Empty
  }

  def filterFR(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h).append(t)) // ????

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A] (a: A) : Stream[A] = ???
  def from(n: Int): Stream[Int] =  cons(n, from(n+1))
  def fibs(): Stream[Int] =  {
    def fib(f0: Int, f1: Int): Stream[Int] = {
      cons(f0+f1, fib(f1, f1+f0))
    }
    cons(0, cons(1, fib(0,1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def produce(s: S) : Stream[A] = {
      val state: Option[(A, S)] = f(s)
      state match {
        case None => empty[A]
        case Some((a, t)) => cons(a, produce(t))
      }
    }
    produce(z)
  }

  def unfoldR[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((h,t)) => cons(h, unfoldR(t)(f))
  }

  def fibsUF(): Stream[Int] = unfoldR((0,1))(s => Some((s._1,(s._2, s._1+s._2))))
}

object Main extends App {
  def f(x: => Int): Int = x
  var stream = Stream(1,2,3)
  val str = Stream(f(1), f(2), f(3))
  stream.takeWhile(x => x > 2)
  println(str.takeWhile(x => x > 2))
  println(stream.take(2).toList)
  println(fibsUF().take(5).toList)
  println(fibs().take(15).toList)
}