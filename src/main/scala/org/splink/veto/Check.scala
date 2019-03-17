package org.splink.veto

trait Check[T] {
  def field[U](f: T => U, name: String)(v: Validator[U])(implicit parent: Option[Context] = None): Check[T]
  def validate: Xor[T]
}

object Check {

  private class CheckImpl[T](t: T, private[this] val xs: List[T => Xor[_]] = Nil) extends Check[T] {

    override def field[U](f: T => U, name: String)(v: Validator[U])(implicit parent: Option[Context] = None): Check[T] = {
      val nameChain = parent.map(_.field + '.').getOrElse("") + name
      val withContext: U => Xor[U] = u => v(u, Context(t, t.getClass.getSimpleName, nameChain, u))
      val pipeline = f andThen withContext

      new CheckImpl(t, pipeline :: xs)
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

  def apply[T](t: T): Check[T] = new CheckImpl[T](t, Nil)
}