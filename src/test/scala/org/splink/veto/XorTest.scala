package org.splink.veto

import org.scalatest.{FlatSpec, Matchers}

class XorTest extends FlatSpec with Matchers {

  val emptyContext = Context("", "", "")

  "Xor" should "map valid values" in {
    val result = Valid("value").map { value =>
      value + 123
    }

    result should equal(Valid("value123"))
  }

  it should "produce the Invalid instance when mapping Invalid values" in {
    val result = Invalid(Error(emptyContext, 'error)).map { _ =>
      123
    }

    result should equal(Invalid(Error(emptyContext, 'error)))
  }

  it should "flatMap valid values" in {
    val result = Valid("value1").flatMap { value1 =>
      Valid("value2").map { value2 =>
        value1 + value2
      }
    }

    result should equal(Valid("value1value2"))
  }

  it should "produce the Invalid instance when flatMapping Invalid values #1" in {
    val result = Valid("value1").flatMap { value1 =>
      Invalid(Error(emptyContext, 'error)).map { value2 =>
        value1 + value2
      }
    }

    result should equal(Invalid(Error(emptyContext, 'error)))
  }

  it should "produce the Invalid instance when flatMapping Invalid values #2" in {
    val result = Invalid(Error(emptyContext, 'error)).flatMap { _ =>
      Valid("oha").map { value2 =>
        value2
      }
    }

    result should equal(Invalid(Error(emptyContext, 'error)))
  }

  it should "get valid values" in {
    Valid("value").getOrElse("else") should equal("value")
  }

  it should "orElse invalid values" in {
    Invalid(Error(emptyContext, 'error)).getOrElse("else") should equal("else")
  }

  it should "fold success valid values" in {
    Valid("value").fold(_ => "error")(_ => "success") should equal("success")
  }

  it should "fold error valid values" in {
    Invalid(Error(emptyContext, 'error)).fold(_ => "error")(_ => "success") should equal("error")
  }

  it should "produce isValid for valid values" in {
    Valid("yo").isValid should equal(true)
  }

  it should "produce !isValid for invalid values" in {
    Invalid(Error(emptyContext, 'error)).isValid should equal(false)
  }
}