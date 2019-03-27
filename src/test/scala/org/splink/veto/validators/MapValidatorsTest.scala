package org.splink.veto.validators

import org.scalatest.{FlatSpec, Matchers}
import org.splink.veto.{Context, Error, Invalid, Valid}
import org.splink.veto.validators.StringValidators._
import org.splink.veto.validators.NumericValidators._
import org.splink.veto.validators.DsValidators._

class MapValidatorsTest extends FlatSpec with Matchers {
  val emptyContext = Context("", "", "")

  "mapNonEmpty" should "return Valid if the map is not empty" in {
    val map = Map(1 -> "max")

    mapNonEmpty(map) shouldBe
      Valid(map)
  }

  it should "return Invalid if the map empty" in {
    val map = Map.empty[String, String]

    mapNonEmpty(map) shouldBe
      Invalid(Error(emptyContext, 'mapNonEmpty))
  }

  "mapContainsKey" should "return Valid if the map contains the given key" in {
    val map = Map(1 -> "max")

    mapContainsKey(1)(map) shouldBe
      Valid(map)
  }

  it should "return Invalid if the map does not contain the given key" in {
    val map = Map(1 -> "max")

    mapContainsKey(2)(map) shouldBe
      Invalid(Error(emptyContext, 'mapMissingKey, Seq(map, 2)))
  }

  "MapValidator" should "return Valid if the values in a Map are valid" in {
    val map = Map(1 -> "max", 2 -> "anna")

    mapValidator[Int, String](tuple2Value(stringNonEmpty)).apply(map) shouldBe
      Valid(map)
  }

  it should "return Valid if the keys in a Map are valid" in {
    val map = Map(1 -> "max", 2 -> "anna")

    mapValidator[Int, String](tuple2Key(isPositive)).apply(map) shouldBe
      Valid(map)
  }

  it should "return Invalid if one of the values in a Map is invalid" in {
    val map = Map(1 -> "max", 2 -> "")

    mapValidator[Int, String](tuple2Value(stringNonEmpty)).apply(map) shouldBe
      Invalid(Error(
        emptyContext.copy(path = "[2]"), 'stringNonEmpty, Seq.empty)
      )
  }

}
