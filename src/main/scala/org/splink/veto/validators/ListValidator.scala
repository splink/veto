package org.splink.veto.validators

import org.splink.veto._

object ListValidator {

  def nonEmpty[T] = Validator[List[T]] { (xs, context) =>
    if (xs.nonEmpty) Valid(xs)
    else Invalid(Error(context, 'listNonEmpty, xs))
  }

  def apply[T](mv: Validator[T]) = Validator[List[T]] { (xs, context) =>

    def validate(v: Xor[T]) = v match {
      case Valid(b) => Valid(List(b))
      case i: Invalid => withIndex(i, index = -1)
    }

    def withIndex(iv: Invalid, index: Int) = Invalid(iv.errors.map { e =>
      val field = e.context.field.replace(context.field + '.', context.field + s"[${index + 1}].")
      e.copy(context = e.context.copy(field = field))
    }: _*)

    xs match {
      case Nil => Valid(xs)
      case head :: tail =>
        val init = validate(mv(head, context))

        tail.zipWithIndex.foldLeft(init) { case (acc, (next, index)) =>
          mv(next, context) match {
            case Valid(nu) =>
              acc match {
                case Valid(v) => Valid(nu :: v)
                case iv: Invalid => iv
              }
            case iv: Invalid =>
              acc match {
                case Valid(_) => withIndex(iv, index)
                case iv1: Invalid => Invalid(iv1.errors ++ withIndex(iv, index).errors: _*)
              }
          }
        }
    }
  }

}
