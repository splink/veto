package org.splink.veto.validators

import org.splink.veto._

object OptionValidators {

  def optionNonEmpty[T] = Validator[Option[T]] { (opt, context) =>
    if (opt.nonEmpty) Valid(opt)
    else Invalid(Error(context, 'optionNonEmpty, Seq(opt)))
  }

  def optionalStringNonEmpty = Validator[Option[String]] { (opt, context) =>
    opt.collect { case s if s.isEmpty =>
      Invalid(Error(context, 'optionNonEmpty))
    } getOrElse {
      Valid(opt)
    }
  }

  def optionalModel[T](mv: ModelValidator[T]) = Validator[Option[T]] { (opt, context) =>
    opt.map(m => mv(m, context).map(Some(_))).getOrElse(Valid(opt))
  }

}
