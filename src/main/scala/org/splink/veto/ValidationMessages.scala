package org.splink.veto

trait ErrorMsg {
  def apply(key: Symbol, args: Any*): String
}

trait ValidationMessages {
  implicit def invalidMsg: ErrorMsg
}

object DefaultValidationMessages extends ValidationMessages {
  val messages = Map[Symbol, String](
    // numeric
    'numericGreaterThan -> "Numeric '{}' must be greater than '{}'.",
    'numericGreaterOrEqual -> "Numeric '{}' must be greater or equal to '{}'.",
    'numericEquals -> "Numeric '{}' must be equal to '{}'.",
    'numericSmallerThan -> "Numeric '{}' must be smaller than '{}'.",
    'numericSmallerOrEqual -> "Numeric '{}' must be smaller or equal to '{}'.",
    'numericIsPositive -> "Numeric '{}' must be positive.",
    // strings
    'stringNonEmpty -> "String '{}' should not be empty.",
    'stringMinLength -> "String '{}' is to short. It should be at least '{}' chars.",
    'stringMaxLength -> "String '{}' is to long. Should not exceed '{}' chars.",
    'stringEquals -> "String '{}' should be equal to '{}'.",
    'stringStartsWith -> "String '{}' should start with '{}'.",
    'stringEndsWith -> "String '{}' should end with '{}'.",
    'stringIsUrl -> "String '{}' is not an URL.",
    'stringIsColor -> "String '{}' is not a Hex-Color representation e.g. #ffffff, #5042f4 or #fff.",
    'stringIsUUID -> "String '{}' is not a valid UUID.",
    // options
    'optionNonEmpty -> "Option should not be empty.",
    // list
    'listNonEmpty -> "List '{}' should not be empty.",
    // map
    'mapNonEmpty -> "Map '{}' should not be empty."
  )


  override implicit def invalidMsg = new ErrorMsg {
    override def apply(key: Symbol, values: Any*) = {
      values.foldLeft(messages(key)) { (acc, next) =>
        acc.replaceFirst("""{}""", next.toString)
      }
    }
  }
}