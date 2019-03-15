package org.splink.veto

trait Validate[T] {
  def field[U](f: T => U, name: String)(v: Validator[U])(implicit parent: Option[Context] = None): Validate[T]
  def validate: Xor[T]
}

object Validate {

  private class ValidateImpl[T](t: T, private[this] val xs: List[T => Xor[_]] = Nil) extends Validate[T] {

    override def field[U](f: T => U, name: String)(v: Validator[U])(implicit parent: Option[Context] = None): Validate[T] = {
      val nameChain = parent.map(_.field + '.').getOrElse("") + name
      val withContext: U => Xor[U] = u => v(u, Context(t, t.getClass.getSimpleName, nameChain, u))
      val pipeline = f andThen withContext

      new ValidateImpl(t, pipeline :: xs)
    }

    override def validate: Xor[T] = {
      val errors = xs.foldLeft(List.empty[Invalid]) { case (acc, next) =>
        next(t) match {
          case Valid(_) => acc
          case iv: Invalid => iv :: acc
        }
      }

      errors match {
        case Nil => Valid(t)
        case errs =>
          Invalid(errs.flatMap(_.errors): _*)
      }
    }
  }

  def apply[T](t: T): Validate[T] = new ValidateImpl[T](t, Nil)
}