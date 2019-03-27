package org.splink.veto

import org.scalatest.{FlatSpec, Matchers}

class ErrorMessagesTest extends FlatSpec with Matchers {
  import DefaultErrorMessages._

  "DefaultErrorMessages.message" should "produce a message for the given key an arguments" in {
    val msg = errorMessage('isGreaterThan, Seq(1, 2))

    msg should equal("'1' should be greater than '2'.")
  }

  it should "produce the key if no error message exists for the key" in {
    val msg = errorMessage('missing, Seq(1, 2))

    msg should equal("'missing")
  }

}
