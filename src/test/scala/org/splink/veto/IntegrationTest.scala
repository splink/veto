package org.splink.veto

import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}
import org.splink.veto.validators.NumericValidators._
import org.splink.veto.validators.StringValidators._
import org.splink.veto.validators.OptionValidator._
import org.splink.veto.validators.DsValidators._

class IntegrationTest extends FlatSpec with Matchers {

  case class Item(id: Option[Id],
                  size: Size,
                  visibility: Visibility,
                  more: List[Item],
                  attrs: Map[String, String] = Map.empty)

  case class Size(width: Int, height: Int)
  case class Id(value: String)

  sealed trait Visibility
  case object Invisible extends Visibility
  case object Visible extends Visibility

  object ItemValidator extends ModelValidator[Item] {
    override def apply(item: Item)(implicit parent: Option[Context]) =
      Check(item)
        .field(_.id, "id")(optional(IdValidator))
        .field(_.size, "size")(SizeValidator)
        .field(item => (item.size, item.visibility), "visibility")(VisibilityValidator)
        .field(_.more, "more")(listValidator(ItemValidator))
        .field(_.attrs, "attrs")(mapValidator(tuple2Key(stringContains("test"))))
        .validate
  }


  def VisibilityValidator = Validator[(Size, Visibility)] {
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

  object IdValidator extends ModelValidator[Id] {
    override def apply(id: Id)(implicit context: Option[Context] = None) = {
      Check(id)
        .field(_.value, "value")(stringNonEmpty and stringIsUUID)
        .validate
    }
  }

  object SizeValidator extends ModelValidator[Size] {
    override def apply(size: Size)(implicit context: Option[Context] = None) = {
      Check(size)
        .field(_.width, "width")(isGreaterThan(-1) and isSmallerOrEqual(12))
        .field(_.height, "height")(isGreaterThan(-1) and isSmallerOrEqual(12))
        .validate
    }
  }

  "ModelValidator[Item]" should "declare a correct model Valid" in {
    val more = Item(Some(Id(UUID.randomUUID().toString)), Size(4, 4), Visible, Nil) :: Nil
    val item = Item(Some(Id(UUID.randomUUID().toString)), Size(4, 4), Visible, more)

    ItemValidator(item) should equal(
      Valid(item)
    )
  }

  it should "output a validation error if a model is Invalid" in {
    val item = Item(Some(Id("123")), Size(4, 4), Visible, Nil, Map("test" -> "world"))

    ItemValidator(item) should equal(
      Invalid(
        Error(Context(item.id.get, "id.value", "123"), 'stringIsUUID, Seq("123"))
      )
    )
  }

  it should "output all validation errors if a model has multiple invalid fields" in {
    val item = Item(Some(Id("")), Size(4, 4), Visible, Nil, Map("hello" -> "world"))

    ItemValidator(item) should equal(
      Invalid(
        Error(Context(item.id.get, "id.value", ""), 'stringNonEmpty, Seq()),
        Error(Context(item.id.get, "id.value", ""), 'stringIsUUID, Seq("")),
        Error(Context(item, "attrs[hello]", item.attrs), 'stringContains, Seq("hello", "test"))
      ))
  }

  it should "output all validation errors if a field is invalid, whose value depends on other fields" in {
    val item = Item(Some(Id(UUID.randomUUID().toString)), Size(0, 0), Visible, Nil)

    ItemValidator(item) should equal(
      Invalid(
        Error(Context(item, "visibility", (Size(0, 0), Visible)), 'visibilitySizeZero, Seq(Visible, Size(0, 0)))
      ))
  }

  it should "output all validation errors if a nested Item is invalid" in {
    val more = Item(Some(Id(UUID.randomUUID().toString)), Size(1, 1), Visible, Nil) ::
      Item(Some(Id(UUID.randomUUID().toString)), Size(0, 0), Visible, Nil) :: Nil
    val item = Item(Some(Id(UUID.randomUUID().toString)), Size(2, 3), Visible, more)

    ItemValidator(item) should equal(
      Invalid(
        Error(
          Context(more.tail.head, "more[1].visibility", (Size(0, 0), Visible)),
          'visibilitySizeZero, Seq(Visible, Size(0, 0)))
      ))
  }

}
