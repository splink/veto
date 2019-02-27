package org.splink.veto.validators

import org.splink.veto._

object MapValidator {

  def nonEmpty[K, V] = Validator[Map[K, V]] { (map, context) =>
    if (map.nonEmpty) Valid(map)
    else Invalid(Error(context, 'mapNonEmpty, Seq(map)))
  }
}
