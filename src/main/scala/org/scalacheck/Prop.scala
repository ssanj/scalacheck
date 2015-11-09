/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2015 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import scala.language.implicitConversions

@scala.scalajs.js.annotation.JSExportDescendentClasses
@scala.scalajs.js.annotation.JSExportDescendentObjects
class Properties(prefix: String) {

  private type P = Gen[(String,Test.Result[String])]

  private val props = new scala.collection.mutable.ListBuffer[P]

  def properties: Seq[P] = props

  /** Adds all properties from another property collection to this one */
  def include(ps: Properties): Unit = include(ps, prefix = "")

  /** Adds all properties from another property collection to this one
   *  with a prefix this is prepended to each included property's name. */
  def include(ps: Properties, prefix: String): Unit =
    props ++= ps.props.map(_.map { case (n,r) => (prefix + n, r) })

  /** Used for specifying properties. Usage:
   *  {{{
   *  property("myProp") = ...
   *  }}}
   */
  class PropertySpecifier() {
    def update[T](propName: String, p: Gen[Prop.Result[T]]) =
      props += Test.check(p.map(_.map(_.toString))).map(
        s"$prefix.$propName" -> _
      )
  }

  val property = new PropertySpecifier()

}

object Prop {

  sealed trait Result[T] {
    def map[U](f: T => U): Result[U]
  }

  case class True[T]() extends Result[T] {
    def map[U](f: T => U) = True[U]()
  }

  case class False[T](ev: T, ev0: Option[T]) extends Result[T] {
    def map[U](f: T => U) = False[U](f(ev), ev0.map(f))
  }

  case class Throw[T](ev: Option[T], e: Throwable) extends Result[T] {
    def map[U](f: T => U) = Throw[U](ev.map(f), e)
  }

  implicit def AnyOps(x: Any) = new {
    def ?==(y: Any): Result[(Any,Any)] =
      if(x == y) True()
      else False(x -> y, None)
  }

  implicit def boolToResult(b: Boolean): Result[Unit] =
    if(b) True() else False((),None)

  implicit def toProp[T,U](t: T)(implicit
    f: T => Result[U]
  ): Gen[Result[U]] = Gen.const(f(t))

  implicit def genToProp[T,U](g: Gen[T])(implicit
    f: T => Result[U]
  ): Gen[Result[U]] = g.map(f)

  def imply[T](c: Boolean)(p: Gen[Result[T]]): Gen[Result[T]] =
    if (c) p else discard

  def succeed[T]: Gen[Result[T]] = True[T]()

  def discard[T]: Gen[Result[T]] = Gen.fail

  def fail: Gen[Result[Unit]] = false

  def forAll[T,U,R](g: Gen[T])(f: T => Gen[R])(implicit r: R => Result[U]
  ): Gen[Result[(T,U)]] = {

    def findSmaller(ts: Stream[T]): Gen[Option[(T,U)]] = {
      if (ts.isEmpty) Gen.const(None)
      else f(ts.head).map(r) flatMap {
        case False(u,u0) => findSmaller(ts.tail) map {
          case None => Some(ts.head -> u0.getOrElse(u))
          case some => some
        }
        case _ => findSmaller(ts.tail)
      }
    }

    g.withShrink flatMap { case (t,ts) =>
      f(t).map(r) flatMap {
        case False(u, u0) => findSmaller(ts) map {
          case None => False((t,u), u0.map(t -> _))
          case some => False((t,u), some)
        }
        case Throw(u, e) => Gen.const(Throw(u.map(t -> _), e))
        case True() => succeed
      }
    }

  }

}
