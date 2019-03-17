package org.splink.veto

trait ModelValidator[T] {
  def apply(t: T)(implicit parent: Option[Context] = None): Xor[T]
}

object ModelValidator {
  implicit def toValidator[T](mv: ModelValidator[T]): Validator[T] = ModelValidator(mv)

  implicit def toModelValidator[T](v: Validator[T]): ModelValidator[T] = new ModelValidator[T] {
    override def apply(t: T)(implicit parent: Option[Context]): Xor[T] = {
      v.apply(t, parent.getOrElse {
        Context(t, "", t)
      })
    }
  }

  def apply[T](mv: ModelValidator[T]) = Validator[T] { (t, context) =>
    mv(t)(Some(context))
  }
}
