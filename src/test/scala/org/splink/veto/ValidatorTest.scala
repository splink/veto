package org.splink.veto

import org.scalatest.{FlatSpec, Matchers}

class ValidatorTest extends FlatSpec with Matchers {

  val emptyContext = Context("", "", "", "")

  val nonEmpty = Validator[String] { (s, context) =>
    if(s.length > 0) Valid(s) else Invalid(Error(context, 'lengthZero), Error(context, 'empty))
  }

  val lowerCase = Validator[String] { (s, context) =>
    if(s.toLowerCase == s) Valid(s) else Invalid(Error(context, 'notLowerCase))
  }

  val longString = Validator[String] { (s, context) =>
    if(s.length > 100) Valid(s) else Invalid(Error(context, 'tooShort), Error(context, 'mustbeVeryLong))
  }

  "Validator" should "return a Valid Instance if the validation succeeds" in {
    nonEmpty("hello", emptyContext) should equal(Valid("hello"))
  }

  it should "return an Invalid Instance if the validation fails" in {
    nonEmpty("", emptyContext) should equal(
      Invalid(Error(emptyContext, 'lengthZero), Error(emptyContext, 'empty)))
  }

  it should "return Valid if two 'and' - combined Validators succeed" in {
    val combined = (nonEmpty and lowerCase)("hello", emptyContext)
    combined should equal(Valid("hello"))
  }

  it should "return Invalid if one of two 'and' - combined Validators fails" in {
    val combined = (nonEmpty and lowerCase)("HELLO", emptyContext)
    combined should equal(Invalid(Error(emptyContext, 'notLowerCase)))
  }

  it should "return Invalid if both of two 'and' - combined Validators fail and include all error messages" in {
    val combined = (longString and lowerCase)("HELLO", emptyContext)
    combined should equal(
      Invalid(Error(emptyContext, 'tooShort), Error(emptyContext, 'mustbeVeryLong), Error(emptyContext, 'notLowerCase)))
  }

  it should "return Invalid if both of two 'or' - combined Validators fail and include all error messages" in {
    val combined = (longString or lowerCase)("HELLO", emptyContext)
    combined should equal(
      Invalid(Error(emptyContext, 'tooShort), Error(emptyContext, 'mustbeVeryLong), Error(emptyContext, 'notLowerCase)))
  }

  it should "return Valid if only one of two 'or' - combined Validators fails" in {
    val combined = (nonEmpty or lowerCase)("HELLO", emptyContext)
    combined should equal(Valid("HELLO"))
  }

}