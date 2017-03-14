package fpinscala.state

import fpinscala.datastructures.Cons


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, nRng) if n == Int.MinValue => (0, nRng)
    case (n, nRng) if n < 0 => (-n, nRng)
    case (n, nRng) => (n, nRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nRnd): (Int, RNG) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nRnd)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, nRng): (Int, RNG) = rng.nextInt
    val (doub, dRng): (Int, RNG) = nonNegativeInt(nRng)
    ((int, doub), dRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (doub, dRng): (Int, RNG) = nonNegativeInt(rng)
    val (int, nRng): (Int, RNG) = dRng.nextInt
    ((doub, int), nRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doub1, dRng1): (Double, RNG) = double(rng)
    val (doub2, dRng2): (Double, RNG) = double(dRng1)
    val (doub3, dRng3): (Double, RNG) = double(dRng2)
    ((doub1, doub2, doub3), dRng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (List(), rng)
    }
    else {
      val (n, nRng): (Int, RNG) = rng.nextInt
      val (list, seed): (List[Int], RNG) = ints(count - 1)(nRng)
      (n :: list, seed)
    }
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveMax(n: Int): Rand[Int] = {
    map[Int, Int](nonNegativeInt)(x => {
      if (x > n) x % n else x
    })
  }

  def doubleMap: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def intDoubleMap: Rand[(Int, Double)] = {
    map2(int, doubleMap)((i, d) => (i, d))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight[Rand[List[A]]](unit(Nil))((randA, randLA) => map2(randA, randLA)((ra, rla) => ra :: rla))

  def ints2(count: Int)(rng: RNG): Rand[List[Int]] = sequence(List.fill(count)(int))

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => rng => (List(), rng)
    case h :: t => rng => ???
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1): (A, RNG) = f(rng)
      g(a)(rng1)
    }
  }

  def nonNegativeIntLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(x => {
    val mod = x % n
    if (x + (n - 1) - mod >= 0) {
      unit(mod)
    } else {
      nonNegativeIntLessThan(n)
    }
  })

  def mapWFM[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))

  def map2WFM1[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = for {
    x <- ra
    y <- rb
  } yield f(x, y)

  def map2WFM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(
    (state: S) => {
      val (a, s): (A, S) = run(state)
      (f(a), s)
    })


  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, newS): (A, S) = run(s)
    f(a).run(newS)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
