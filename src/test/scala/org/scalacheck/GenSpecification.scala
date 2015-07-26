package org.scalacheck

import scala.language.reflectiveCalls

object GenSpecification extends Properties("Gen") {

  import Prop._
  import Gen._

  val gens = oneOf(const(1), oneOf(2,3,4), const(0).repeatedly)

  property("resize") = forAll(oneOf(8,7,6,5)) { sz =>
    size.resize(sz).map(_ ?== sz)
  }

  property("repeat") = forAll(const(0).repeatedly) { _ == 0 }

}
