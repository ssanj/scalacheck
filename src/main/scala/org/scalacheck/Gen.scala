/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2015 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

sealed trait Gen[T] {

  import Gen._

  override def toString = "<GEN>"

  val apply: Parameters => Result[T]

  def mapResult[U](f: Result[T] => Result[U]): Gen[U] = gen(apply.andThen(f))

  def map[U](f: T => U): Gen[U] = mapResult(_.map(f))

  def filter(f: T => Boolean): Gen[T] = mapResult(_.filter(f))

  def sampleResult(seed: Long, size: Int = 100): Result[T] =
    apply(params(seed, size, { new scala.util.Random(_) }))

  def sampleResult: Result[T] = apply(params(0L, 100, { new scala.util.Random(_) }))

  def sample: Option[T] = sampleResult.value

  def sample(seed: Long, size: Int = 100): Option[T] =
    sampleResult(seed, size).value

  def flatMap[U](f: T => Gen[U]): Gen[U] = gen { p =>
    val r1 = apply(p)
    r1.value match {
      case None => res(None, Nil, r1.next.map(_.flatMap(f)))
      case Some(t) =>
        val r2 = f(t).reseed.apply(p)
        new Result[U] {
          val params = p
          val value = r2.value
          lazy val shrink = r2.shrink ++ r1.shrink.flatMap { t =>
            val r = f(t).reseed.apply(p)
            r.value.toSeq ++ r.shrink
          }
          lazy val next = (r1.next, r2.next) match {
            case (Some(gt),Some(gu)) => Some(oneOfGens(gt.flatMap(f), gu))
            case (None,gu) => gu
            case (gt,None) => gt.map(_.flatMap(f))
          }
        }
    }
  }

  def repeatedly: Gen[T] = setNext(repeatedly)

  def repeatedly(f: Gen[T] => Gen[T]): Gen[T] =
    setNext(f(this)).mapNext(_.map(_.repeatedly(f)))

  def setSize(size: Int): Gen[T] =
    gen(p => apply(p.resize(size))).mapNext(_.map(_.setSize(size)))

  def resize(f: Int => Int): Gen[T] = size flatMap (sz => setSize(f(sz)))

  def setSeed(newSeed: Long): Gen[T] = gen(p => apply(p.reseed(newSeed)))

  def reseed: Gen[T] = gen(p => apply(p.reseed))

  def setNext(g: => Gen[T]): Gen[T] = mapNext (_ => Some(g))

  def mapNext(f: Option[Gen[T]] => Option[Gen[T]]): Gen[T] = mapResult { r =>
    res(r.value, r.shrink, f(r.next))
  }

  def terminate: Gen[T] = mapNext (_ => None)

  def withNext: Gen[(Option[T], Option[Gen[T]])] = mapResult { r =>
    res(Some(r.value -> r.next), r.shrink.map(Some(_) -> r.next), None)
  }

  def followedBy(g: Gen[T]): Gen[T] = withNext flatMap {
    case (t,None) => fromOption(t) setNext g
    case (t,Some(g0)) => fromOption(t) setNext g0.followedBy(g)
  }

  def withShrink: Gen[(T, () => Seq[T])] = mapResult { r =>
    res(r.value.map(_ -> (() => r.shrink)),
      r.shrink.map(_ -> (() => r.shrink)), r.next.map(_.withShrink))
  }

  def noShrink: Gen[T] = setShrink(_ => Nil)

  def setShrink(f: T => Seq[T]): Gen[T] = mapResult { r =>
    res(r.value, r.value.map(f).getOrElse(Nil), r.next)
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
      var nextGen: Option[Gen[T]] = Some(Gen.this)
      def hasNext = nextGen.isDefined
      def next() = {
        val r = nextGen.get.apply(p)
        nextGen = r.next
        r
      }
    })
  }

}

object Gen {

  sealed trait Parameters {
    val seed: Long
    val size: Int
    def resize(newSize: Int): Parameters
    def reseed: Parameters
    def reseed(newSeed: Long): Parameters
  }

  def params(seed: Long, size: Int, rngf: Long => scala.util.Random): Parameters = {
    case class P(seed: Long, size: Int) extends Parameters {
      lazy val reseed = copy(seed = rngf(seed).nextLong)
      def reseed(newSeed: Long) = copy(seed = newSeed)
      def resize(newSize: Int) = copy(size = newSize)
    }
    P(seed, size)
  }

  sealed trait Result[T] {
    def value: Option[T]
    def shrink: Seq[T]
    def next: Option[Gen[T]]

    def map[U](f: T => U): Result[U] = new Result[U] {
      def value = Result.this.value.map(f)
      def shrink = Result.this.shrink.map(f)
      def next = Result.this.next.map(_.map(f))
    }

    def filter(f: T => Boolean): Result[T] = new Result[T] {
      def value = Result.this.value.filter(f)
      def shrink = if(value.isDefined) Result.this.shrink else Nil
      def next = Result.this.next.map(_.filter(f))
    }
  }

  private def gen[T](f: Parameters => Result[T]): Gen[T] = new Gen[T] {
    val apply = f
  }

  private def res[T](
    v: Option[T], s: => Seq[T] = Nil, n: => Option[Gen[T]] = None
  ): Result[T] = new Result[T] {
    val value = v
    def shrink = s
    def next = n
  }

  def fail[T]: Gen[T] = fromOption(None)

  def const[T](t: T): Gen[T] = fromOption(Some(t))

  def fromOption[T](o: Option[T]): Gen[T] = gen(_ => res(o))

  def parameterized[T](f: Parameters => Gen[T]): Gen[T] = gen(p => f(p).apply(p))

  val size: Gen[Int] = parameterized(p => const(p.size))

  val seed: Gen[Long] = parameterized(p => const(p.seed))

  def seed(mod: Long): Gen[Long] = seed map (_ % mod)

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

  private def dropIdx[T](xs: Traversable[T], idx: Int): Seq[T] = {
    val b = new collection.immutable.VectorBuilder[T]
    var i = 0
    xs.foreach { x => if (i != idx) { b += x }; i += 1 }
    b.result
  }

  def oneOfGens[T](gs: Seq[Gen[T]]): Gen[T] =
    if (gs.isEmpty) fail
    else gen { prms =>
      val idx = (math.abs(prms.seed) % gs.size).toInt
      val rest = dropIdx(gs, idx)
      val r = gs(idx).apply(prms)
      new Result[T] {
        val params = prms
        val value = r.value
        def shrink = r.shrink
        def next = (r.next,rest.isEmpty) match {
          case (g,true) => g
          case (None,false) => Some(oneOfGens(rest))
          case (Some(g),false) => Some(oneOfGens(g +: rest))
        }
      }
    }

  def oneOfGens[T](g0: Gen[T], gn: Gen[T]*): Gen[T] = oneOfGens(g0 +: gn)

  def oneOf[T](xs: Seq[T]): Gen[T] =
    if(xs.isEmpty) fail
    else gen { prms =>
      val idx = (math.abs(prms.seed) % xs.size).toInt
      val rest = dropIdx(xs, idx)
      new Result[T] {
        val params = prms
        val value = Some(xs(idx))
        def shrink = xs.take(idx).reverse
        def next = if(rest.isEmpty) None else Some(oneOf(rest))
      }
    }

  def oneOf[T](x0: T, xs: T*): Gen[T] = oneOf(x0 +: xs)
}
