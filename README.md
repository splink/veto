# Veto
A scala validation library without dependencies.

### Let the code speak:

#### Model:
Just a simple case class structure to demonstrate validation
~~~scala
case class Item(id: Option[Id],
                  size: Size,
                  visibility: Visibility,
                  more: List[Item],
                  attrs: Map[String, String])

case class Size(width: Int, height: Int)
case class Id(value: String)

sealed trait Visibility
case object Invisible extends Visibility
case object Visible extends Visibility

~~~

#### Validators:
~~~scala
import org.splink.veto.validators.NumericValidators._
import org.splink.veto.validators.StringValidators._
import org.splink.veto.validators.OptionValidator._
import org.splink.veto.validators.DsValidators._

// extend the 'ModelValidator' tait to validate the fields of a case class
object ItemValidator extends ModelValidator[Item] {
    override def apply(item: Item)(implicit parent: Option[Context]) =
      Check(item)
        .field(_.id, "id")(optional(IdValidator))
        .field(_.size, "size")(SizeValidator)
        // validator which depends on the values of two different fields
        .field(item => (item.size, item.visibility), "visibility")(VisibilityValidator)
        // validate a self reference
        .field(_.more, "more")(listValidator(ItemValidator))
        // tuple2Key or tuple2Value can be used to determine whether to pick key or value to validate
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
      // validators can be chained with 'and' and 'or' 
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
~~~

#### Usage:
~~~scala
// you can implement your own error messages by implementing the 'ErrorMessages' trait
import DefaultErrorMessages._

val more = Item(Some(Id(UUID.randomUUID().toString)), Size(0, 0), Visible, List.empty, Map.empty) :: Nil
val item = Item(Some(Id(UUID.randomUUID().toString)), Size(4, 4), Visible, more, Map("test" -> "it"))


ItemValidator(item) match {
  case Valid(t) => 
    println(s"a valid $t")
  case iv: Invalid =>
    iv.errors.foreach(e => println(e.message))
}
~~~
The Invalid instance contains all validation errors. Each Error has a Context. This Context provides the path to the field where the validation failed and other useful information.
In the example the path to the failed visibility field would be for instance: ```more[0].visibility```.


#### Make your own Validator

It's quick to create a custom Validator:
~~~scala
def stringContains(value: String) = Validator[String] { (s, context) =>
    if (s.contains(value)) Valid(s)
    else Invalid(Error(context, 'stringContains, Seq(s, value)))
  }
~~~
Context is taken care of upstream, namely Check does the job when it is used to declare which fields of a class are to be validated.
So you just need to perform the check for validity. If everything is fine, return Valid with the value and if not, return Invalid. Invalid requires the context (from upstream), as well as an error message key and a Seq of values which are to be used in the error message.
Like ```String'{}' should contain '{}'.```   