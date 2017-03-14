package fpinscala.errorhandling


import scala.collection.mutable.ListBuffer
import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[C](f: A => C): Option[C] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[D>:A](default: => D): D = this match {
    case None => default
    case Some(x) => x
  }

  def flatMapPM[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  // Flatten the structure....map returns a nested structure Eg: Option(Option of) but we need to flatten it out to get just of type Option

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None) // map(f) Option(Option(B))

  def orElse1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(x) => this
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(x => Some(x)).getOrElse(ob)


  def filter1(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }

  def filter(f: A => Boolean): Option[A] = flatMap(x => {
    if(f(x)) Some(x) else None
  })
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =  {
    val mn: Option[Double] = mean(xs)
    mn.flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)))
  def map2ForComprehensions[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    x <- a
    y <- b
  } yield f(x,y)

//  def sequence[A](list: List[Option[A]]): Option[List[A]] = {
//    var result: ListBuffer[A] = new ListBuffer[A]()
//    list.foreach(x => this match {
//      case Some(x) =>  result.:+(x)
//      case _ => return None
//    })
//    Some(result.toList)
//  }

//  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a.map(f))

  def sequenceViaTraverse[A](list: List[Option[A]]): Option[List[A]] = ???

    object Main extends App {
    println (Some(1).map(_ => 2))
    println (None.map(_ => 2))
    println (None.getOrElse(2))
    println(Some(2).flatMap(x => Some(x*x)))
    println(Some(2).orElse(Some(3)))
    println(None.orElse(Some(3)))
    println(Some(4).filter(x => x > 3))
    println(mean(Seq(1.9,2.9,3.9)))
  }
}