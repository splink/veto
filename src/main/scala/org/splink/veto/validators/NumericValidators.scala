package org.splink.veto.validators

import Numeric.Implicits._
import Ordering.Implicits._
import org.splink.veto._

object NumericValidators {

  def isPositive[T : Numeric] = Validator[T] { (i, context) =>
    if (i.abs() == i) Valid(i)
    else Invalid(Error(context, 'isPositive, Seq(i)))
  }

  def isGreaterThan[T : Numeric](value: T) = Validator[T] { (i, context) =>
    if (i > value) Valid(i)
    else Invalid(Error(context, 'isGreaterThan, Seq(i, value)))
  }

  def isGreaterOrEqual[T : Numeric](value: T) = Validator[T] { (i, context) =>
    if (i >= value) Valid(i)
    else Invalid(Error(context, 'isGreaterOrEqual,  Seq(i, value)))
  }

  def isSmallerThan[T : Numeric](value: T) = Validator[T] { (i, context) =>
    if (i < value) Valid(i)
    else Invalid(Error(context, 'isSmallerThan,  Seq(i, value)))
  }

  def isSmallerOrEqual[T : Numeric](value: T) = Validator[T] { (i, context) =>
    if (i <= value) Valid(i)
    else Invalid(Error(context, 'isSmallerOrEqual,  Seq(i, value)))
  }

  def isEqual[T : Numeric](value: T) = Validator[T] { (i, context) =>
    if (i == value) Valid(i)
    else Invalid(Error(context, 'isEqual,  Seq(i, value)))
  }

}
