package org.splink.veto

trait Check[T] {
  def field[U](f: T => U, name: String)(v: Validator[U]): Check[T]
  def validate: Xor[T]
}

object Check {

  private class CheckImpl[T](t: T, private[this] val fields: List[T => Xor[_]] = Nil)(
    implicit parent: Option[Context] = None) extends Check[T] {


    override def field[U](f: T => U, name: String)(v: Validator[U]): Check[T] = {
      val path = parent.map(_.path + '.').getOrElse("") + name
      val withContext: U => Xor[U] = u => v(u, Context(t, path, u))
      val pipeline = f andThen withContext

      new CheckImpl(t, pipeline :: fields)
    }

    override def validate: Xor[T] = {
      val errors = fields.foldLeft(List.empty[Invalid]) { case (acc, next) =>
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

  def apply[T](t: T)(implicit parent: Option[Context] = None): Check[T] = new CheckImpl[T](t, Nil)
}