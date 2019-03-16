package org.splink.veto

import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}
import org.splink.veto.validators.NumericValidators._
import org.splink.veto.validators.StringValidators._

class IntegrationTest extends FlatSpec with Matchers {

  case class Item(id: Id, size: Size, visibility: Visibility)

  case class Size(width: Int, height: Int)

  case class Id(value: String)

  sealed trait Visibility

  case object Invisible extends Visibility

  case object Visible extends Visibility

  object ItemValidator extends ModelValidator[Item] {
    override def apply(item: Item)(implicit parent: Option[Context]) =
      Validate(item)
        .field(_.id, "item.id")(IdValidator)
        .field(_.size, "item.size")(SizeValidator)
        .field(item => (item.size, item.visibility), "item.visibility")(VisibilityValidator.apply)
        .validate
  }

  object VisibilityValidator {
    def apply = Validator[(Size, Visibility)] {
      case ((size, visibility), context) =>
        visibility match {
          case Invisible =>
            Valid(size, visibility)
          case Visible if size.width > 0 && size.height > 0 =>
            Valid(size, visibility)
          case _ => Invalid(
            Error(context, 'visibilitySizeZero, Seq(visibility, size))
          )
        }
    }
  }

  object IdValidator extends ModelValidator[Id] {
    override def apply(id: Id)(implicit parent: Option[Context] = None) =
      Validate(id)
        .field(_.value, "value")(stringNonEmpty and stringIsUUID)
        .validate
  }

  object SizeValidator extends ModelValidator[Size] {
    override def apply(size: Size)(implicit context: Option[Context] = None) = {
      Validate(size)
        .field(_.width, "width")(isGreaterThan(-1) and isSmallerOrEqual(12))
        .field(_.height, "height")(isGreaterThan(-1) and isSmallerOrEqual(12))
        .validate
    }
  }


  "ModelValidator[Item]" should "declare a correct model Valid" in {
    val item = Item(Id(UUID.randomUUID().toString), Size(4, 4), Visible)

    ItemValidator(item) should equal(
      Valid(item)
    )
  }

  it should "output a validation error if a model is Invalid" in {
    val item = Item(Id("123"), Size(4, 4), Visible)

    ItemValidator(item) should equal(
      Invalid(
        Error(Context(item.id, "Id", "item.id.value", "123"), 'stringIsUUID, Seq("123"))
      )
    )
  }

  it should "output all validation errors if a model has multiple invalid fields" in {
    val item = Item(Id(""), Size(4, 4), Visible)

    ItemValidator(item) should equal(
      Invalid(
        Error(Context(item.id, "Id", "item.id.value", ""), 'stringNonEmpty, Seq("")),
        Error(Context(item.id, "Id", "item.id.value", ""), 'stringIsUUID, Seq(""))
      ))
  }

  it should "output all validation errors if a field is invalid, whose value depends on other fields" in {
    val item = Item(Id(UUID.randomUUID().toString), Size(0, 0), Visible)

    ItemValidator(item) should equal(
      Invalid(
        Error(Context(item, "Item", "item.visibility", (Size(0, 0), Visible)), 'visibilitySizeZero, Seq(Visible, Size(0, 0)))
      ))
  }

}
