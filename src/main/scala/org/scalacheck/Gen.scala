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

sealed trait Gen[T] {

  import Gen._

  override def toString = "<GEN>"

  val apply: Parameters => Result[T]

  def mapResult[U](f: Result[T] => Result[U]): Gen[U] = gen(apply.andThen(f))

  def map[U](f: T => U): Gen[U] = mapResult(_.map(f))

  def contramap(f: Parameters => Parameters): Gen[T] = gen(p => apply(f(p)))

  def filter(f: T => Boolean): Gen[T] = mapResult(_.filter(f))

  def sampleResult(seed: Int = 0, size: Int = 100): Result[T] =
    apply(Parameters(Seed(seed), size))

  def sample(seed: Int = 0, size: Int = 100): Option[T] =
    sampleResult(seed, size).value

  def flatMap[U](f: T => Gen[U]): Gen[U] = gen { p =>
    val rt = apply(p)
    rt.value match {
      case None => res(rt.nextSeed, None, Nil, rt.next.map(_.flatMap(f)))
      case Some(t) =>
        val ru = f(t).apply(p.copy(seed = rt.nextSeed))
        new Result[U] {
          val value = ru.value
          val nextSeed = ru.nextSeed
          lazy val shrink = ru.shrink ++ rt.shrink.flatMap { t =>
            val r = f(t).apply(p.copy(seed = rt.nextSeed))
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

  def repeatedly: Gen[T] = setNext(repeatedly)

  def resize(size: Int): Gen[T] = contramap(_.copy(size = size))

  def resize(f: Int => Int): Gen[T] = size flatMap (sz => resize(f(sz)))

  def setNext(g: => Gen[T]): Gen[T] = setNextOpt(Some(g))

  def setNextOpt(g: => Option[Gen[T]]): Gen[T] = mapNext(_ => g)

  def mapNext(f: Option[Gen[T]] => Option[Gen[T]]): Gen[T] = mapResult { r =>
    res(r.nextSeed, r.value, r.shrink, f(r.next))
  }

  def withNext: Gen[(Option[T], Option[Gen[T]])] = mapResult { r =>
    res(r.nextSeed, Some(r.value -> r.next), r.shrink.map(Some(_) -> r.next), None)
  }

  def followBy(g: => Gen[T]): Gen[T] = mapNext {
    case None => Some(g)
    case Some(g0) => Some(g0 followBy g)
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

  def last = iterate.map { it =>
    var r: Option[T] = None
    while (it.hasNext) r = it.next.value
    r
  }

  def iterate: Gen[Iterator[Result[T]]] = parameterized { p =>
    const(new Iterator[Result[T]] {
      var r = res(p.seed, None, Nil, Some(Gen.this))
      def hasNext = r.next.isDefined
      def next() = {
        r = r.next.get.apply(p.copy(seed = r.nextSeed))
        r
      }
    })
  }

}

object Gen {

  case class Parameters(seed: Seed, size: Int)

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

  private def gen[T](f: Parameters => Result[T]): Gen[T] = new Gen[T] {
    val apply = f
  }

  private def res[T](
    seed: Seed, v: Option[T], s: => Seq[T] = Nil, n: => Option[Gen[T]] = None
  ): Result[T] = new Result[T] {
    val value = v
    val nextSeed = seed
    def shrink = s
    def next = n
  }

  def fail[T]: Gen[T] = fromOption(None)

  def const[T](t: T): Gen[T] = fromOption(Some(t))

  def fromOption[T](o: Option[T]): Gen[T] = gen(p => res(p.seed, o))

  def parameterized[T](f: Parameters => Gen[T]): Gen[T] = gen(p => f(p).apply(p))

  val size: Gen[Int] = parameterized(p => const(p.size))

  val long: Gen[Long] = gen { p =>
    val (r, nextSeed) = Rng.long(p.seed)
    res(nextSeed, Some(r))
  }

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

  def oneOfGens[T](g0: Gen[T], gn: Gen[T]*): Gen[T] = oneOfGens(g0 +: gn)

  def oneOf[T](ts: Seq[T]): Gen[T] =
    if(ts.isEmpty) fail
    else long flatMap { n =>
      val idx = (math.abs(n) % ts.size).toInt
      val (t, rest) = dropIdx(ts, idx)
      const(t) setNextOpt {
        if(rest.isEmpty) None else Some(oneOf(rest))
      } setShrink { _ => ts.take(idx).reverse }
    }

  def oneOf[T](x0: T, xs: T*): Gen[T] = oneOf(x0 +: xs)
}
