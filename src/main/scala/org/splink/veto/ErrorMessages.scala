package org.splink.veto

trait ErrorMsg {
  def apply(key: Symbol, args: Seq[Any]): String
}

trait ErrorMessages {
  implicit def errorMessage: ErrorMsg
}

object DefaultErrorMessages extends ErrorMessages {
  val messages = Map[Symbol, String](
    // numeric
    'isGreaterThan -> "'{}' should be greater than '{}'.",
    'isGreaterOrEqual -> "'{}' should be greater or equal to '{}'.",
    'isEqual -> "'{}' should be equal to '{}'.",
    'isSmallerThan -> "'{}' should be smaller than '{}'.",
    'isSmallerOrEqual -> "'{}' should be smaller or equal to '{}'.",
    'issPositive -> "'{}' should be positive.",
    // strings
    'stringNonEmpty -> "String should not be empty.",
    'stringMinLength -> "String '{}' is to short. It should be at least '{}' chars.",
    'stringMaxLength -> "String '{}' is to long. Should not exceed '{}' chars.",
    'stringEquals -> "String '{}' should be equal to '{}'.",
    'stringStartsWith -> "String '{}' should start with '{}'.",
    'stringEndsWith -> "String '{}' should end with '{}'.",
    'stringContains -> "String'{}' should contain '{}'.",
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
    'listMissingElement -> "List '{} should contain the element '{}'."
  )


  override implicit def errorMessage = new ErrorMsg {
    override def apply(key: Symbol, args: Seq[Any]) = {
      messages.get(key).map { message =>
        args.foldLeft(message) { (acc, next) =>
          acc.replaceFirst("\\{\\}", next.toString)
        }
      }.getOrElse {
        key.toString
      }
    }
  }
}