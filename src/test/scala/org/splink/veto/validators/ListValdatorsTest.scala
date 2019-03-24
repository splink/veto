package org.splink.veto.validators

import org.scalatest.{FlatSpec, Matchers}
import org.splink.veto._
import org.splink.veto.validators.ListValidator._
import org.splink.veto.validators.OptionValidator._
import org.splink.veto.validators.StringValidators._

class ListValdatorsTest extends FlatSpec with Matchers {
  val emptyContext = Context("", "", "")

  "ListValidator.nonEmpty" should "return Valid if the List is not empty" in {
    nonEmpty.apply(List(1, 2, 3), emptyContext) shouldBe Valid(List(1, 2, 3))
  }

  it should "return Invalid if the List is empty" in {
    nonEmpty.apply(Nil, emptyContext) shouldBe
      Invalid(Error(emptyContext, 'listNonEmpty, Nil))
  }

  "ListValidator" should "return Valid if the List contains multiple Valid elements" in {
    ListValidator(stringNonEmpty)(List("hello", "world", "!"), emptyContext) shouldBe
      Valid(List("hello", "world", "!"))
  }

  it should "return Valid if the List contains one Valid element" in {
    ListValidator(stringNonEmpty)(List("hello"), emptyContext) shouldBe Valid(List("hello"))
  }

  it should "return Valid if the List is empty" in {
    ListValidator(stringNonEmpty)(Nil, emptyContext) shouldBe Valid(Nil)
  }

  it should "return Invalid if the first element in the List is Invalid" in {
    ListValidator(stringNonEmpty)(List("", "world"), emptyContext) shouldBe
      Invalid(Error(emptyContext.copy(path = "[0]"), 'stringNonEmpty, ""))
  }

  it should "return Invalid if one of the elements in the List is Invalid" in {
    ListValidator(stringNonEmpty)(List("hello", ""), emptyContext) shouldBe
      Invalid(Error(emptyContext.copy(path = "[1]"), 'stringNonEmpty, ""))
  }

  it should "return Valid if a nested List contains valid elements" in {
    ListValidator(ListValidator(stringNonEmpty))(List(List("hello", "world", "!")), emptyContext) shouldBe
      Valid(List(List("hello", "world", "!")))
  }

  it should "return Valid if a nested List contains one valid element" in {
    ListValidator(ListValidator(stringNonEmpty))(List(List("hello")), emptyContext) shouldBe
      Valid(List(List("hello")))
  }

  it should "return Valid if a nested List is empty" in {
    ListValidator(ListValidator(stringNonEmpty))(List(Nil), emptyContext) shouldBe
      Valid(List(Nil))
  }

  it should "return Invalid if the first element in the nested List is Invalid" in {
    ListValidator(ListValidator(stringNonEmpty))(List(List("")), emptyContext) shouldBe
      Invalid(Error(emptyContext.copy(path = "[0][0]"), 'stringNonEmpty, ""))
  }

  case class Container(xs: List[List[String]], opt: Option[List[String]], xso: List[Option[String]])

  object ContainerValidator extends ModelValidator[Container] {
    override def apply(t: Container)(implicit parent: Option[Context] = None) = {
      Check(t)
        .field(_.xs, "xs")(ListValidator(ListValidator(stringNonEmpty)))
        .field(_.opt, "opt")(optional(ListValidator(stringNonEmpty)))
        .field(_.xso, "xso")(ListValidator(optional(stringNonEmpty)))
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
    val lh = Container(Nil, Some(List("hello", "")), Nil)
    ContainerValidator(lh) should equal(
      Invalid(Error(Context(lh, "opt[1]", lh.opt), 'stringNonEmpty, Seq.empty))
    )
  }

  it should "return Invalid if an optional element in a List is invalid" in {
    val lh = Container(Nil, None, List(None, Some("world"), Some("")))
    ContainerValidator(lh) should equal(
      Invalid(Error(Context(lh, "xso[2]", lh.xso), 'stringNonEmpty, Seq.empty))
    )
  }
}