package org.splink.veto.validators

import org.splink.veto._

object DsValidators {

  import StructLikes._

  def listNonEmpty[T] = Validator[List[T]] { (xs, context) =>
    if (xs.nonEmpty) Valid(xs)
    else Invalid(Error(context, 'listNonEmpty, xs))
  }

  def listContainsElement[T](element: T) = Validator[List[T]] { (xs, context) =>
    if (xs.contains(element)) Valid(xs)
    else Invalid(Error(context, 'listMissingElement, Seq(xs, element)))
  }

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

  def listValidator[T](validator: Validator[T]) =
    apply[List[T], T](validator)(structLikeList[T])

  def mapValidator[K, V](validator: Validator[(K, V)]) =
    apply[Map[K, V], (K, V)](validator)(structLikeMap[K, V])

  private def apply[F, T](validator: Validator[T])(implicit ev: StructLike[F, T]) = Validator[F] { (ds, context) =>

    def updatePath(iv: Invalid, key: String) = Invalid(iv.errors.map { e =>
      val currentPath = context.path
      val newPath = context.path + s"[$key]"
      val field = if (currentPath.length > 0 && e.context.path.contains(currentPath)) {
        e.context.path.replace(currentPath, newPath)
      } else {
        e.context.path + newPath
      }

      e.copy(context = e.context.copy(path = field))
    }: _*)

    ds match {
      case x if ev.isEmpty(x) => Valid(ds)
      case x =>
        val init = validator(ev.head(x), context) match {
          case Valid(t) => Valid(ev.create(t))
          case i: Invalid => updatePath(i, ev.keyFor(x, ev.head(x), index = 0))
        }

        val (result, _) = ev.foldLeft(ev.tail(x))(init -> 1) { case ((acc, index), next) =>
          val xor = validator(next, context) match {
            case Valid(t) =>
              acc match {
                case Valid(v) => Valid(ev.add(v, t))
                case iv: Invalid => iv
              }
            case iv: Invalid =>
              acc match {
                case Valid(_) =>
                  updatePath(iv, ev.keyFor(x, next, index))
                case iv1: Invalid =>
                  val iv2 = updatePath(iv, ev.keyFor(x, next, index))
                  Invalid(iv1.errors ++ iv2.errors: _*)
              }
          }
          xor -> (index + 1)
        }

        result
    }
  }

  private trait StructLike[F, T] {
    def create(t: T): F
    def head(f: F): T
    def tail(f: F): F
    def foldLeft[X](f: F)(init: X)(op: (X, T) => X): X
    def add(f: F, t: T): F
    def isEmpty(f: F): Boolean
    def keyFor(f:F, t: T, index: Int): String
  }

  private object StructLikes {

    implicit def structLikeMap[K, V] = new StructLike[Map[K, V], (K, V)] {
      type F = Map[K, V]
      type T = (K, V)

      override def create(t: T): F = Map(t)
      override def head(f: F): T = f.head
      override def tail(f: F): F = f.tail
      override def foldLeft[X](f: F)(init: X)(op: (X, T) => X): X = f.foldLeft(init)(op)
      override def add(f: F, t: T): F = f + t
      override def isEmpty(f: F): Boolean = f.isEmpty
      override def keyFor(f:F, t: T, index: Int): String = t._1.toString
    }

    implicit def structLikeList[T] = new StructLike[List[T], T] {
      type F = List[T]

      override def create(t: T): F = List(t)
      override def head(f: F): T = f.head
      override def tail(f: F): F = f.tail
      override def foldLeft[X](f: F)(init: X)(op: (X, T) => X): X = f.foldLeft(init)(op)
      override def add(f: F, t: T): F = f :+ t
      override def isEmpty(f: F): Boolean = f.isEmpty
      override def keyFor(f: F, t: T, index: Int): String = index.toString
    }
  }
}