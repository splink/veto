package org.splink.veto

final case class Context(instance: Any, path: String, value: Any) {
  def typ = instance.getClass.getSimpleName
}

trait Validator[T] { self =>

  def apply(t: T, c: Context): Xor[T]
  def apply(value: T): Xor[T] = apply(value, Context("", "", ""))

  def and(v: => Validator[T]) = new Validator[T] {
    override def apply(t: T, c: Context): Xor[T] = self(t, c) match {
      case Valid(_) => v(t, c) match {
        case result@Valid(_) => result
        case i2: Invalid => i2
      }
      case i1: Invalid => v(t, c) match {
        case i2: Invalid => Invalid(i1.errors ++ i2.errors: _*)
        case _ => i1
      }
    }
  }

  def or(v: => Validator[T]) = new Validator[T] {
    override def apply(t: T, c: Context): Xor[T] = self(t, c) match {
      case r@Valid(_) => r
      case i1: Invalid => v(t, c) match {
        case r@Valid(_) => r
        case i2: Invalid =>
          Invalid(i1.errors ++ i2.errors: _*)
      }
    }
  }
}

object Validator {
  def apply[T](f: (T, Context) => Xor[T]) = new Validator[T] {
    override def apply(value: T, context: Context) = f(value, context)
  }


  def valid[T] = new Validator[T] {
    override def apply(value: T, context: Context): Xor[T] = Valid(value)
  }
}