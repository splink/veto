package org.splink.veto.validators

import org.splink.veto._

object OptionValidators {

  def optionNonEmpty[T] = Validator[Option[T]] { (opt, context) =>
    if (opt.nonEmpty) Valid(opt)
    else Invalid(Error(context, 'optionNonEmpty, Seq.empty))
  }

  def optional[T](v: Validator[T]) = Validator[Option[T]] { (opt, context) =>
    opt.map { t =>
      v(t, context).map(t => Some(t))
    } getOrElse {
      Valid(opt)
    }
  }

  def optionalModel[T](mv: ModelValidator[T]) = Validator[Option[T]] { (opt, context) =>
    opt.map { m =>
      mv(m, context).map(Some(_))
    }.getOrElse {
      Valid(opt)
    }
  }

}
