package org.splink.veto.validators

import org.scalatest.{FlatSpec, Matchers}
import org.splink.veto._
import org.splink.veto.validators.OptionValidator._
import org.splink.veto.validators.StringValidators._
import org.splink.veto.validators.DsValidators._

class ListValidatorsTest extends FlatSpec with Matchers {
  val emptyContext = Context("", "", "")

  "listNonEmpty" should "return Valid if the List is not empty" in {
    listNonEmpty(List(1, 2, 3)) shouldBe Valid(List(1, 2, 3))
  }

  it should "return Invalid if the List is empty" in {
    listNonEmpty(Nil) shouldBe
      Invalid(Error(emptyContext, 'listNonEmpty, Nil))
  }

  "listContainsElement" should "return Valid if the list contains the given element" in {
    val xs = List("max")

    listContainsElement("max")(xs) shouldBe
      Valid(xs)
  }

  it should "return Invalid if the list does not contain the given element" in {
    val xs = List("max")

    listContainsElement("anna")(xs) shouldBe
      Invalid(Error(emptyContext, 'listMissingElement, Seq(xs, "anna")))
  }

  "ListValidator" should "return Valid if the List contains multiple Valid elements" in {
    listValidator[String](stringNonEmpty)(List("hello", "world", "!")) shouldBe
      Valid(List("hello", "world", "!"))
  }

  it should "return Valid if the List contains one Valid element" in {
    listValidator[String](stringNonEmpty)(List("hello")) shouldBe Valid(List("hello"))
  }

  it should "return Valid if the List is empty" in {
    listValidator[String](stringNonEmpty)(Nil) shouldBe Valid(Nil)
  }

  it should "return Invalid if the first element in the List is Invalid" in {
    listValidator[String](stringNonEmpty)(List("", "world")) shouldBe
      Invalid(Error(emptyContext.copy(path = "[0]"), 'stringNonEmpty, ""))
  }

  it should "return Invalid if one of the elements in the List is Invalid" in {
    listValidator[String](stringNonEmpty)(List("hello", "world", "")) shouldBe
      Invalid(Error(emptyContext.copy(path = "[2]"), 'stringNonEmpty, ""))
  }

  it should "return Valid if a nested List contains valid elements" in {
    listValidator[List[String]](listValidator[String](stringNonEmpty))(List(List("hello", "world", "!"))) shouldBe
      Valid(List(List("hello", "world", "!")))
  }

  it should "return Valid if a nested List contains one valid element" in {
    listValidator[List[String]](listValidator[String](stringNonEmpty))(List(List("hello"))) shouldBe
      Valid(List(List("hello")))
  }

  it should "return Valid if a nested List is empty" in {
    listValidator[List[String]](listValidator[String](stringNonEmpty))(List(Nil)) shouldBe
      Valid(List(Nil))
  }

  it should "return Invalid if the first element in the nested List is Invalid" in {
    listValidator[List[String]](listValidator[String](stringNonEmpty))(List(List(""))) shouldBe
      Invalid(Error(emptyContext.copy(path = "[0][0]"), 'stringNonEmpty, ""))
  }

  case class Container(xs: List[List[String]], opt: Option[List[String]], xso: List[Option[String]])

  object ContainerValidator extends ModelValidator[Container] {
    override def apply(t: Container)(implicit parent: Option[Context] = None) = {
      Check(t)
        .field(_.xs, "xs")(listValidator[List[String]](listValidator[String](stringNonEmpty)))
        .field(_.opt, "opt")(optional(listValidator[String](stringNonEmpty)))
        .field(_.xso, "xso")(listValidator[Option[String]](optional(stringNonEmpty)))
        .validate
    }
  }

  it should "return Invalid with two errors if two elements in a nested List are invalid" in {
    val lh = Container(List(List("hello", "", "")), None, Nil)
    ContainerValidator(lh) should equal(
      Invalid(
        Error(Context(lh, "xs[0][1]", lh.xs), 'stringNonEmpty, Seq.empty),
        Error(Context(lh, "xs[0][2]", lh.xs), 'stringNonEmpty, Seq.empty)
      )
    )
  }

  it should "return Invalid if an element in a List which is nested in an Option is invalid" in {
    val lh = Container(Nil, Some(List("hello", "world", "", "!")), Nil)
    ContainerValidator(lh) should equal(
      Invalid(Error(Context(lh, "opt[2]", lh.opt), 'stringNonEmpty, Seq.empty))
    )
  }

  it should "return Invalid if an optional element in a List is invalid" in {
    val lh = Container(Nil, None, List(None, Some("world"), Some("")))
    ContainerValidator(lh) should equal(
      Invalid(Error(Context(lh, "xso[2]", lh.xso), 'stringNonEmpty, Seq.empty))
    )
  }
}