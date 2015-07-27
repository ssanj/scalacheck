package org.scalacheck

import scala.language.reflectiveCalls

object GenSpecification extends Properties("Gen") {

  import Prop._
  import Gen._

  property("resize") = forAll(oneOf(8,7,6,5)) { sz =>
    size.resize(sz).map(_ ?== sz)
  }

}
