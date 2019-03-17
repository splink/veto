package org.splink.veto

import org.scalatest.{FlatSpec, Matchers}
import org.splink.veto.validators.NumericValidators._

class NumericValidatorsTest extends FlatSpec with Matchers {
  val emptyContext = Context("", "", "")

  "numericIsPositive" should "return Valid, if 1 >= 0" in {
    isPositive[Int].apply(1, emptyContext) shouldBe Valid(1)
  }

  it should "return Valid, if 0 >= 0" in {
    isPositive[Int].apply(0, emptyContext) shouldBe Valid(0)
  }

  it should "return Invalid, if -1 >= 0" in {
    isPositive[Int].apply(-1, emptyContext) shouldBe
      Invalid(Error(emptyContext, 'isPositive, Seq(-1)))
  }

  "numericIsPositive" should "return Valid, if 1.5f >= 0" in {
    isPositive[Float].apply(1.5f, emptyContext) shouldBe Valid(1.5f)
  }

  it should "return Invalid, if -1.25f >= 0" in {
    isPositive[Float].apply(-1.25f, emptyContext) shouldBe
      Invalid(Error(emptyContext, 'isPositive, Seq(-1.25f)))
  }

  "numericGreaterThan" should "return Valid, if 2 > 1'" in {
    isGreaterThan[Int](1).apply(2, emptyContext) shouldBe Valid(2)
  }

  it should "return Invalid, if 2 >= 2'" in {
    isGreaterThan[Int](2).apply(2, emptyContext) shouldBe
      Invalid(Error(emptyContext, 'isGreaterThan, Seq(2, 2)))
  }

  it should "return Invalid, if 1 >= 2'" in {
    isGreaterThan[Int](2).apply(1, emptyContext) shouldBe
      Invalid(Error(emptyContext, 'isGreaterThan, Seq(1, 2)))
  }

  "isGreaterOrEqual" should "return Valid, if 2 >= 2" in {
    isGreaterOrEqual[Int](2).apply(2, emptyContext) shouldBe Valid(2)
  }

  it should "return Invalid, if 1 >= 2" in {
    isGreaterOrEqual[Int](2).apply(1, emptyContext) shouldBe
      Invalid(Error(emptyContext, 'isGreaterOrEqual, Seq(1, 2)))
  }

  "isSmallerThan" should "return Valid, if 1 < 2" in {
    isSmallerThan[Int](2).apply(1, emptyContext) shouldBe Valid(1)
  }

  it should "return InValid, if 2 < 2" in {
    isSmallerThan[Int](2).apply(2, emptyContext) shouldBe
      Invalid(Error(emptyContext, 'isSmallerThan, Seq(2, 2)))
  }

  "isSmallerOrEqual" should "return Valid, if 2 <= 2" in {
    isSmallerOrEqual[Int](2).apply(2, emptyContext) shouldBe Valid(2)
  }

  it should "return InValid, if 3 < 2" in {
    isSmallerOrEqual[Int](2).apply(3, emptyContext) shouldBe
      Invalid(Error(emptyContext, 'isSmallerOrEqual, Seq(3, 2)))
  }

  "isEqual" should "return Valid, if 2 == 2" in {
    isEqual[Int](2).apply(2, emptyContext) shouldBe Valid(2)
  }

  it should "return InValid, if 3 == 2" in {
    isEqual[Int](2).apply(3, emptyContext) shouldBe
      Invalid(Error(emptyContext, 'isEqual, Seq(3, 2)))
  }

}
