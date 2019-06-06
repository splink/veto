package org.splink.veto

import scala.language.implicitConversions

trait ModelValidator[T] {
  def apply(t: T)(implicit parent: Option[Context] = None): Xor[T]
}

object ModelValidator {
  implicit def toValidator[T](mv: ModelValidator[T]): Validator[T] = Validator[T] { (t, context) =>
    mv(t)(Some(context))
  }
}
