package org.splink.veto

trait ErrorMsg {
  def apply(key: Symbol, args: Any*): String
}

trait ErrorMessages {
  implicit def errorMessage: ErrorMsg
}

object DefaultErrorMessages extends ErrorMessages {
  val messages = Map[Symbol, String](
    // numeric
    'isGreaterThan -> "'{}' must be greater than '{}'.",
    'isGreaterOrEqual -> "'{}' must be greater or equal to '{}'.",
    'isEqual -> "'{}' must be equal to '{}'.",
    'isSmallerThan -> "'{}' must be smaller than '{}'.",
    'isSmallerOrEqual -> "'{}' must be smaller or equal to '{}'.",
    'issPositive -> "'{}' must be positive.",
    // strings
    'stringNonEmpty -> "String should not be empty.",
    'stringMinLength -> "String '{}' is to short. It should be at least '{}' chars.",
    'stringMaxLength -> "String '{}' is to long. Should not exceed '{}' chars.",
    'stringEquals -> "String '{}' should be equal to '{}'.",
    'stringStartsWith -> "String '{}' should start with '{}'.",
    'stringEndsWith -> "String '{}' should end with '{}'.",
    'stringIsUrl -> "String '{}' is not an URL.",
    'stringIsHexColor -> "String '{}' is not a Hex-Color representation e.g. #ffffff, #5042f4 or #fff.",
    'stringIsUUID -> "String '{}' is not a valid UUID.",
    // options
    'optionNonEmpty -> "Option should not be empty.",
    'optionStringNonEmpty -> "The String inside the option should not be empty.",
    // list
    'listNonEmpty -> "List '{}' should not be empty.",
    // map
    'mapNonEmpty -> "Map '{}' should not be empty.",
    'mapMissingKey -> "Map '{}' should contain the key '{}'.",
    'listMissingElement -> "List '{} should contain the element '{}'"
  )


  override implicit def errorMessage = new ErrorMsg {
    override def apply(key: Symbol, values: Any*) = {
      values.foldLeft(messages(key)) { (acc, next) =>
        acc.replaceFirst("""{}""", next.toString)
      }
    }
  }
}