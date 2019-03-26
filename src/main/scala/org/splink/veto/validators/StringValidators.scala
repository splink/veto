package org.splink.veto.validators

import java.util.UUID
import java.util.regex.Pattern

import org.splink.veto._

import scala.util.Try

object StringValidators {
  def stringNonEmpty = Validator[String] { (s, context) =>
    if (s.length > 0) Valid(s)
    else Invalid(Error(context, 'stringNonEmpty, Seq.empty))
  }

  def stringMinLength(value: Int = 0) = Validator[String] { (s, context) =>
    if (s.length >= value) Valid(s)
    else Invalid(Error(context, 'stringMinLength, Seq(s, value)))
  }

  def stringMaxLength(value: Int = 0) = Validator[String] { (s, context) =>
    if (s.length <= value) Valid(s)
    else Invalid(Error(context, 'stringMaxLength, Seq(s, value)))
  }

  def stringEquals(value: String) = Validator[String] { (s, context) =>
    if (s == value) Valid(s)
    else Invalid(Error(context, 'stringEquals, Seq(s, value)))
  }

  def stringStartsWith(value: String) = Validator[String] { (s, context) =>
    if (s.startsWith(value)) Valid(s)
    else Invalid(Error(context, 'stringStartsWith, Seq(s, value)))
  }

  def stringEndsWith(value: String) = Validator[String] { (s, context) =>
    if (s.endsWith(value)) Valid(s)
    else Invalid(Error(context, 'stringEndsWith, Seq(s, value)))
  }

  def stringContains(value: String) = Validator[String] { (s, context) =>
    if (s.contains(value)) Valid(s)
    else Invalid(Error(context, 'stringContains, Seq(s, value)))
  }

  private val urlPattern = Pattern.compile("""^(http|https):\/\/[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(\/\S*)?$""")

  def stringIsUrl = Validator[String] { (s, context) =>
    if (urlPattern.matcher(s).matches) Valid(s)
    else Invalid(Error(context, 'stringIsUrl, Seq(s)))
  }

  private val hexColorPattern = Pattern.compile("""^#([A-Fa-f0-9]{2,8})$""")

  def stringIsHexColor = Validator[String] { (s, context) =>
    if(hexColorPattern.matcher(s).matches) Valid(s)
    else Invalid(Error(context, 'stringIsHexColor, Seq(s)))
  }

  def stringIsUUID = Validator[String] { (s, context) =>
    Try(UUID.fromString(s)).map(_ => Valid(s)).getOrElse(Invalid(Error(context, 'stringIsUUID, Seq(s))))
  }
}
