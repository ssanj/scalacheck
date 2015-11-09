package org.scalacheck

import scala.language.reflectiveCalls

object GenSpecification extends Properties("Gen") {

  import Prop.{ forAll, AnyOps }
  import Gen._

  private val generators = oneOf(
    int,
    long,
    const(1),
    oneOf(2,3),
    oneOf(1,2,3),
    oneOfGens(const(1), size, choose(1,10))
  )

  property("resize") = forAll(generators) { g =>
    forAll(oneOf(4,5,6,7)) { sz =>
      resize(sz, g).flatMap(_ => size ).map(_ ?== sz)
    }
  }

}
