/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2015 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import rng.{Seed, Rng}

sealed abstract class Gen[T] {

  import Gen._

  //// Public interface ////

  /** A class supporting filtered operations. */
  final class WithFilter(p: T => Boolean) {
    def map[U](f: T => U): Gen[U] = Gen.this.suchThat(p).map(f)
    def flatMap[U](f: T => Gen[U]): Gen[U] = Gen.this.suchThat(p).flatMap(f)
    def withFilter(q: T => Boolean): WithFilter = Gen.this.withFilter(x => p(x) && q(x))
  }

  /** Evaluate this generator with the given parameters */
  def apply(p: Parameters): Result[T]

  /** Create a new generator by mapping the result of this generator */
  def map[U](f: T => U): Gen[U] = mapResult(_.map(f))

  /** Create a new generator by mapping the result of this generator */
  def mapResult[U](f: Result[T] => Result[U]): Gen[U] = gen(apply _ andThen f)

  /** Create a new generator by mapping the input to this generator */
  def contramap(f: Parameters => Parameters): Gen[T] = gen(p => apply(f(p)))

  /** Create a new generator by flat-mapping the result of this generator */
  def flatMap[U](f: T => Gen[U]): Gen[U] = gen { p =>
    val rt = apply(p)
    rt.value match {
      case None => res(rt.nextSeed, None, Nil, rt.next.map(_.flatMap(f)))
      case Some(t) =>
        val ru = f(t).apply(p.withSeed(rt.nextSeed))
        new Result[U] {
          val value = ru.value
          val nextSeed = ru.nextSeed
          lazy val shrink = ru.shrink ++ rt.shrink.flatMap { t =>
            val r = f(t).apply(p.withSeed(rt.nextSeed))
            r.value.toSeq ++ r.shrink
          }
          lazy val next = (rt.next, ru.next) match {
            case (Some(gt),Some(gu)) => Some(oneOfGens(gt.flatMap(f), gu))
            case (None,gu) => gu
            case (gt,None) => gt.map(_.flatMap(f))
          }
        }
    }
  }

  /** Create a new generator that uses this generator to produce a value
   *  that fulfills the given condition. If the condition is not fulfilled,
   *  the generator fails (returns None). Also, make sure that the provided
   *  test property is side-effect free, eg it should not use external vars. */
  def filter(p: T => Boolean): Gen[T] = suchThat(p)

  /** Create a new generator that fails if the specified partial function
   *  is undefined for this generator's value, otherwise returns the result
   *  of the partial function applied to this generator's value. */
  def collect[U](pf: PartialFunction[T,U]): Gen[U] =
    flatMap { t => Gen.fromOption(pf.lift(t)) }

  /** Creates a non-strict filtered version of this generator. */
  def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)

  /** Create a new generator that uses this generator to produce a value
   *  that fulfills the given condition. If the condition is not fulfilled,
   *  the generator fails (returns None). Also, make sure that the provided
   *  test property is side-effect free, eg it should not use external vars.
   *  This method is identical to [Gen.filter]. */
  def suchThat(f: T => Boolean): Gen[T] = mapResult(_.filter(f))

  /** Create a generator that calls this generator repeatedly until
   *  the given condition is fulfilled. The generated value is then
   *  returned. Use this combinator with care, since it may result
   *  in infinite loops. Also, make sure that the provided test property
   *  is side-effect free, eg it should not use external vars. */
  def retryUntil(p: T => Boolean): Gen[T] = gen { p0 =>
    var r = apply(p0)
    while (!r.value.map(p).getOrElse(false))
      r = apply(p0.withSeed(r.nextSeed))
    r
  }

  def sample: Option[T] = apply(Parameters.default).value

  def repeatedly: Gen[T] = setNext(repeatedly)

  def setNext(g: => Gen[T]): Gen[T] = setNextOpt(Some(g))

  def setNextOpt(g: => Option[Gen[T]]): Gen[T] = mapNext(_ => g)

  def mapNext(f: Option[Gen[T]] => Option[Gen[T]]): Gen[T] = mapResult { r =>
    res(r.nextSeed, r.value, r.shrink, f(r.next))
  }

  def withNext: Gen[(Option[T], Option[Gen[T]])] = mapResult { r =>
    res(r.nextSeed, Some(r.value -> r.next), r.shrink.map(Some(_) -> r.next), None)
  }

  def followedBy(g: => Gen[T]): Gen[T] = mapNext {
    case None => Some(g)
    case Some(g0) => Some(g0 followedBy g)
  }

  def withShrink: Gen[(T, () => Seq[T])] = mapResult { r =>
    res(r.nextSeed, r.value.map(_ -> (() => r.shrink)),
      r.shrink.map(_ -> (() => r.shrink)), r.next.map(_.withShrink))
  }

  def noShrink: Gen[T] = setShrink(_ => Nil)

  def setShrink(f: T => Seq[T]): Gen[T] = mapResult { r =>
    res(r.nextSeed, r.value, r.value.map(f).getOrElse(Nil), r.next)
  }

  def unfold: Gen[Seq[T]] =
    iterate.map(_.flatMap(_.value.toList).toList)

  def take(n: Int): Gen[Seq[T]] =
    iterate.map(_.take(n).flatMap(_.value.toList).toList)

  def iterate: Gen[Iterator[Result[T]]] = parameterized { p =>
    const(new Iterator[Result[T]] {
      var r = res(p.seed, None, Nil, Some(Gen.this))
      def hasNext = r.next.isDefined
      def next() = {
        r = r.next.get.apply(p.withSeed(r.nextSeed))
        r
      }
    })
  }

  override def toString: String = "<GEN>"

}

object Gen {

  //// Private interface ////

  /** Helper */
  private def dropIdx[T](xs: Traversable[T], idx: Int): (T,Seq[T]) = {
    val b = new collection.immutable.VectorBuilder[T]
    var i = 0
    var t: Option[T] = None
    xs.foreach { x =>
      if (i != idx) b += x
      else t = Some(x)
      i += 1
    }
    (t.get, b.result)
  }

  private def dropIdx[T](xs: Traversable[T], idx: Int): (T,Seq[T]) = {
    val b = new collection.immutable.VectorBuilder[T]
    var i = 0
    var t: Option[T] = None
    xs.foreach { x =>
      if (i != idx) b += x
      else t = Some(x)
      i += 1
    }
    (t.get, b.result)
  }

  private def gen[T](f: Parameters => Result[T]): Gen[T] = new Gen[T] {
    def apply(p: Parameters) = f(p)
  }

  private def res[T](
    seed: Seed, v: Option[T], s: => Seq[T] = Nil, n: => Option[Gen[T]] = None
  ): Result[T] = new Result[T] {
    val value = v
    val nextSeed = seed
    def shrink = s
    def next = n
  }

  //// Public interface ////

  /** Generator parameters, used by [[org.scalacheck.Gen.apply]] */
  sealed abstract class Parameters {

    /** The size of the generated value. Generator implementations are allowed
     *  to freely interpret (or ignore) this value. During test execution, the
     *  value of this parameter is controlled by [[Test.Parameters.minSize]] and
     *  [[Test.Parameters.maxSize]]. */
    val size: Int

    val seed: Seed

    /** Create a copy of this [[Gen.Parameters]] instance with
     *  [[Gen.Parameters.size]] set to the specified value. */
    def withSize(size: Int): Parameters = cp(size = size)

    /** Create a copy of this [[Gen.Parameters]] instance with
     *  [[Gen.Parameters.size]] set to the specified value. */
    def withSeed(seed: Seed): Parameters = cp(seed = seed)

    // private since we can't guarantee binary compatibility for this one
    private case class cp(
      size: Int = size, seed: Seed = seed
    ) extends Parameters
  }

  /** Provides methods for creating [[org.scalacheck.Gen.Parameters]] values */
  object Parameters {
    /** Default generator parameters instance. */
    def default: Parameters = new Parameters {
      val size: Int = 100
      val seed: Seed = Seed.random
    }
  }

  sealed abstract class Result[T] {
    def value: Option[T]
    def nextSeed: Seed
    def shrink: Seq[T]
    def next: Option[Gen[T]]

    def map[U](f: T => U): Result[U] = new Result[U] {
      def value = Result.this.value.map(f)
      def nextSeed = Result.this.nextSeed
      def shrink = Result.this.shrink.map(f)
      def next = Result.this.next.map(_.map(f))
    }

    def filter(f: T => Boolean): Result[T] = new Result[T] {
      def value = Result.this.value.filter(f)
      def nextSeed = Result.this.nextSeed
      def shrink = if(value.isDefined) Result.this.shrink else Nil
      def next = Result.this.next.map(_.filter(f))
    }
  }

  /** A wrapper type for range types */
  trait Choose[T] {
    /** Creates a generator that returns a value in the given inclusive range */
    def choose(min: T, max: T): Gen[T]
  }

  /** Provides implicit [[org.scalacheck.Gen.Choose]] instances */
  object Choose {

    // TODO
    private def shrinkLong(lowerBound: Long, n: Long): List[Long] = Nil
    private def shrinkDouble(lowerBound: Double, n: Double): List[Double] = Nil

    private def chLng(l: Long, h: Long)(p: Parameters): Result[Long] = {
      if (h < l) res(p.seed, None) else {
        val d = h - l + 1
        if (d <= 0) {
          var (n,s) = Rng.long(p.seed)
          while (n < l || n > h) {
            val (n1,s1) = Rng.long(s)
            n = n1
            s = s1
          }
          res(s, Some(n), shrinkLong(l, n))
        } else {
          val (n, s) = Rng.long(p.seed)
          val v = l + (n & 0x7fffffffffffffffL) % d
          res(s, Some(v), shrinkLong(l, v))
        }
      }
    }

    private def chDbl(l: Double, h: Double)(p: Parameters): Result[Double] = {
      val d = h-l
      if (d < 0 || d > Double.MaxValue) res(p.seed, None)
      else if (d == 0) res(p.seed, Some(l))
      else {
        val (n, s) = Rng.double(p.seed)
        val v = n * (h-l) + l
        res(s, Some(v), shrinkDouble(l, v))
      }
    }

    implicit val chooseLong: Choose[Long] = new Choose[Long] {
      def choose(low: Long, high: Long) =
        gen(chLng(low,high)).repeatedly
    }
    implicit val chooseInt: Choose[Int] = new Choose[Int] {
      def choose(low: Int, high: Int) =
        gen(chLng(low,high)).map(_.toInt).repeatedly
    }
    implicit val chooseByte: Choose[Byte] = new Choose[Byte] {
      def choose(low: Byte, high: Byte) =
        gen(chLng(low,high)).map(_.toByte).repeatedly
    }
    implicit val chooseShort: Choose[Short] = new Choose[Short] {
      def choose(low: Short, high: Short) =
        gen(chLng(low,high)).map(_.toShort).repeatedly
    }
    implicit val chooseChar: Choose[Char] = new Choose[Char] {
      def choose(low: Char, high: Char) =
        gen(chLng(low,high)).map(_.toChar).repeatedly
    }
    implicit val chooseDouble: Choose[Double] = new Choose[Double] {
      def choose(low: Double, high: Double) =
        gen(chDbl(low,high)).repeatedly
    }
    implicit val chooseFloat: Choose[Float] = new Choose[Float] {
      def choose(low: Float, high: Float) =
        gen(chDbl(low,high)).map(_.toFloat).repeatedly
    }

    /** Transform a Choose[T] to a Choose[U] where T and U are two isomorphic
     *  types whose relationship is described by the provided transformation
     *  functions. (exponential functor map) */
    def xmap[T, U](from: T => U, to: U => T)(implicit c: Choose[T]): Choose[U] =
      new Choose[U] {
        def choose(low: U, high: U) = c.choose(to(low), to(high)).map(from)
      }
  }


  //// Various Generator Combinators ////

  /** A generator that always generates the given value */
  def const[T](t: T): Gen[T] = fromOption(Some(t))

  /** A generator that never generates a value */
  def fail[T]: Gen[T] = fromOption(None)

  /** A generator that fails if the provided option value is undefined,
   *  otherwise just returns the value. */
  def fromOption[T](o: Option[T]): Gen[T] = gen(p => res(p.seed, o))

  /** A generator that generates a random value in the given (inclusive)
   *  range. If the range is invalid, the generator will not generate
   *  any value. */
  def choose[T](min: T, max: T)(implicit c: Choose[T]): Gen[T] =
    c.choose(min, max)

  /** Wraps a generator lazily. The given parameter is only evaluated once,
   *  and not until the wrapper generator is evaluated. */
  def lzy[T](g: => Gen[T]): Gen[T] = {
    lazy val h = g
    delay(h)
  }

  /** Wraps a generator for later evaluation. The given parameter is
   *  evaluated each time the wrapper generator is evaluated. */
  def delay[T](g: => Gen[T]) = gen { p => g.apply(p) }

  /** Creates a generator that can access its generation parameters */
  def parameterized[T](f: Parameters => Gen[T]): Gen[T] = gen(p => f(p).apply(p))

  /** Creates a generator that can access its generation size */
  def sized[T](f: Int => Gen[T]) = size flatMap f

  /** A generator that returns the current generation size */
  val size: Gen[Int] = parameterized(p => const(p.size))

  /** Creates a resized version of a generator */
  def resize[T](s: Int, g: Gen[T]) = g.contramap(_.withSize(s))

  /** Picks a random generator from a list */
  def oneOfGens[T](gs: Seq[Gen[T]]): Gen[T] =
    if (gs.isEmpty) fail
    else long flatMap { n =>
      val idx = (math.abs(n) % gs.size).toInt
      val (g, rest) = dropIdx(gs, idx)
      if(rest.isEmpty) g
      else g mapNext {
        case None => Some(oneOfGens(rest))
        case Some(g0) => Some(oneOfGens(g0 +: rest))
      }
    }

  /** Picks a random generator from a list */
  def oneOfGens[T](g0: Gen[T], gn: Gen[T]*): Gen[T] = oneOfGens(g0 +: gn)

  /** Picks a random value from a list */
  def oneOf[T](ts: Seq[T]): Gen[T] =
    if(ts.isEmpty) fail
    else long flatMap { n =>
      val idx = (math.abs(n) % ts.size).toInt
      val (t, rest) = dropIdx(ts, idx)
      const(t) setNextOpt {
        if(rest.isEmpty) None else Some(oneOf(rest))
      } setShrink { _ => ts.take(idx).reverse }
    }

  /** Picks a random value from a list */
  def oneOf[T](x0: T, xs: T*): Gen[T] = oneOf(x0 +: xs)

  /** Makes a generator result optional. Either `Some(T)` or `None` will be provided. */
  def option[T](g: Gen[T]): Gen[Option[T]] =
    oneOfGens(const[Option[T]](None), g.map[Option[T]](Some.apply))

  /** Generates a uniformly distributed Long value */
  val long: Gen[Long] = gen { p =>
    val (r, nextSeed) = Rng.long(p.seed)
    res(nextSeed, Some(r))
  }

  /** Generates a uniformly distributed Double value */
  val double: Gen[Double] = gen { p =>
    val (r, nextSeed) = Rng.double(p.seed)
    res(nextSeed, Some(r))
  }

  def zip[T1,T2](g1: Gen[T1], g2: Gen[T2]
  ): Gen[(T1,T2)] = g1.flatMap(t1 => g2.map((t1,_)))

  def zip[T1,T2,T3](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3]
  ): Gen[(T1,T2,T3)] = zip(g1,g2).flatMap(t => g3.map((t._1,t._2,_)))

  def zip[T1,T2,T3,T4](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4]
  ): Gen[(T1,T2,T3,T4)] = zip(g1,g2,g3).flatMap(t =>
    g4.map((t._1,t._2,t._3,_))
  )

  def zip[T1,T2,T3,T4,T5](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4],
    g5: Gen[T5]
  ): Gen[(T1,T2,T3,T4,T5)] = zip(g1,g2,g3,g4).flatMap(t =>
    g5.map((t._1,t._2,t._3,t._4,_))
  )
}
