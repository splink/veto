package org.splink.veto

sealed trait Xor[+A] { self =>

  def map[T](f: A => T): Xor[T] = self match {
    case Valid(r) => Valid(f(r))
    case iv: Invalid => iv
  }

  def flatMap[T](f: A => Xor[T]): Xor[T] = self match {
    case Valid(r) => f(r)
    case iv: Invalid => iv
  }

  def getOrElse[T >: A](alt: => T): T = self match {
    case Valid(r) => r
    case _: Invalid => alt
  }

  def fold[T](alt: Invalid => T)(f: A => T): T = self match {
    case Valid(r) => f(r)
    case iv: Invalid => alt(iv)
  }

  def isValid: Boolean = self.map(_ => true).getOrElse(false)
}

final case class Error(context: Context, errorCode: Symbol, args: Seq[Any] = Seq.empty) {
  def message(implicit msg: ErrorMsg): String = msg(errorCode, args)

}
final case class Invalid(errors: Error*) extends Xor[Nothing]
final case class Valid[T](t: T) extends Xor[T]