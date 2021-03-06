package org.splink.veto.validators

import org.scalatest.{FlatSpec, Matchers}
import org.splink.veto.validators.NumericValidators._
import org.splink.veto.validators.OptionValidator._
import org.splink.veto.{Context, Error, Invalid, Valid}

class OptionValidatorTest extends FlatSpec with Matchers {

  val emptyContext = Context("", "", "")

  "optional" should "validate the value wrapped in an Option and succeed if it is valid" in {
    val validator = optional[Int](isPositive[Int])
    validator(Some(1)) should equal(Valid(Some(1)))
  }

  it should "validate the value wrapped in an Option and fail if it is invalid" in {
    val validator = optional[Int](isPositive[Int])
    validator(Some(-1)) should equal(
      Invalid(
        Error(emptyContext, 'isPositive, Seq(-1))
      )
    )
  }

  it should "fail if combined with a optionNonEmpty" in {
    val validator = optional[Int](isPositive[Int]) and optionNonEmpty
    validator(None) should equal(
      Invalid(
        Error(emptyContext, 'optionNonEmpty, Seq())
      )
    )
  }
}
