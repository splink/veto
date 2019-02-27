package org.splink.veto.validators

import Numeric.Implicits._
import Ordering.Implicits._
import org.splink.veto._

object NumericValidators {

  def numericIsPositive[T : Numeric] = Validator[T] { (i, context) =>
    if (i.abs() == i) Valid(i)
    else Invalid(Error(context, 'numericIsPositive, Seq(i)))
  }

  def numericGreaterThan[T : Numeric](value: T = 0) = Validator[T] { (i, context) =>
    if (i > value) Valid(i)
    else Invalid(Error(context, 'numericGreaterThan, Seq(i, value)))
  }

  def numericGreaterOrEqual[T : Numeric](value: T = 0) = Validator[T] { (i, context) =>
    if (i >= value) Valid(i)
    else Invalid(Error(context, 'numericGreaterOrEqual,  Seq(i, value)))
  }

  def numericSmallerThan[T : Numeric](value: T = 0) = Validator[T] { (i, context) =>
    if (i < value) Valid(i)
    else Invalid(Error(context, 'numericSmallerThan,  Seq(i, value)))
  }

  def numericSmallerOrEqual[T : Numeric](value: T = 0) = Validator[T] { (i, context) =>
    if (i <= value) Valid(i)
    else Invalid(Error(context, 'numericSmallerOrEqual,  Seq(i, value)))
  }

  def numericEquals[T : Numeric](value: T = 0) = Validator[T] { (i, context) =>
    if (i == value) Valid(i)
    else Invalid(Error(context, 'numericEquals,  Seq(i, value)))
  }

}
