package org.splink.veto

import org.scalatest.{FlatSpec, Matchers}

class MacroTest extends FlatSpec with Matchers {


  case class Name(name: String)

  class Test[T](t: T) {
    def field[U](f: T => U) = {
      val r = f(t)

      println("~~~> " + Macro.body(this))
      r
    }
  }


  "macro" should "behave" in {

    

    new Test(Name("max")).field(_.name)


    1 should equal(1)
  }

}
