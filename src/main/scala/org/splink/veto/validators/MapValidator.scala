package org.splink.veto.validators

import org.splink.veto._

object MapValidator {

  def mapNonEmpty[K, V] = Validator[Map[K, V]] { (map, context) =>
    if (map.nonEmpty) Valid(map)
    else Invalid(Error(context, 'mapNonEmpty, Seq.empty))
  }

  def mapContainsKey[K, V](key: K) = Validator[Map[K, V]] { (map, context) =>
    if (map.contains(key)) Valid(map)
    else Invalid(Error(context, 'mapMissingKey, Seq(map, key)))
  }

  def tuple2Key[K, V](validator: Validator[K]) = Validator[(K, V)] { case ((k, v), context) =>
    validator(k, context).map(k => k -> v)
  }

  def tuple2Value[K, V](validator: Validator[V]) = Validator[(K, V)] { case ((k, v), context) =>
    validator(v, context).map(v => k -> v)
  }

  def apply[K, V](validator: Validator[(K, V)]) = Validator[Map[K, V]] { (m, context) =>

    def updatePath(iv: Invalid, key: K) = Invalid(iv.errors.map { e =>
      val currentPath = context.path
      val newPath = context.path + s"[${key.toString}]"
      val field = if (currentPath.length > 0 && e.context.path.contains(currentPath)) {
        e.context.path.replace(currentPath, newPath)
      } else {
        e.context.path + newPath
      }

      e.copy(context = e.context.copy(path = field))
    }: _*)

    m match {
      case mm if mm.isEmpty => Valid(m)
      case mm =>

        val init = validator(mm.head, context) match {
          case Valid(x) => Valid(Map(x))
          case i: Invalid => updatePath(i, mm.head._1)
        }

        mm.tail.foldLeft(init) { case (acc, next) =>
          validator(next, context) match {
            case Valid(nu) =>
              acc match {
                case Valid(v) => Valid(v + nu)
                case iv: Invalid => iv
              }
            case iv: Invalid =>
              acc match {
                case Valid(_) => updatePath(iv, next._1)
                case iv1: Invalid => Invalid(iv1.errors ++ updatePath(iv, next._1).errors: _*)
              }
          }
        }
    }
  }
}
