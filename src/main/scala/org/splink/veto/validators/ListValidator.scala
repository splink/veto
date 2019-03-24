package org.splink.veto.validators

import org.splink.veto._

object ListValidator {

  def nonEmpty[T] = Validator[List[T]] { (xs, context) =>
    if (xs.nonEmpty) Valid(xs)
    else Invalid(Error(context, 'listNonEmpty, xs))
  }

  def apply[T](validator: Validator[T]) = Validator[List[T]] { (xs, context) =>

    def validate(v: Xor[T]) = v match {
      case Valid(x) => Valid(List(x))
      case i: Invalid => updatePath(i, index = 0)
    }

    def updatePath(iv: Invalid, index: Int) = Invalid(iv.errors.map { e =>
      val currentPath = context.path
      val newPath = context.path + s"[$index]"
      val field = if (currentPath.length > 0 && e.context.path.contains(currentPath)) {
        e.context.path.replace(currentPath, newPath)
      } else {
        e.context.path + newPath
      }

      e.copy(context = e.context.copy(path = field))
    }: _*)

    xs match {
      case Nil => Valid(xs)
      case head :: tail =>
        val init = validate(validator(head, context))

        tail.zipWithIndex.foldLeft(init) { case (acc, (next, index)) =>
          validator(next, context) match {
            case Valid(nu) =>
              acc match {
                case Valid(v) if index == tail.size - 1 =>
                  Valid((nu :: v).reverse)
                case Valid(v) =>
                  Valid(nu :: v)
                case iv: Invalid => iv
              }
            case iv: Invalid =>
              acc match {
                case Valid(_) => updatePath(iv, index + 1)
                case iv1: Invalid => Invalid(iv1.errors ++ updatePath(iv, index + 1).errors: _*)
              }
          }
        }
    }
  }

}
