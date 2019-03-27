# Veto
A scala validation library without dependencies.

### Let the code speak:

##### Validate a simple case class
~~~scala
case class Size(width: Int, height: Int)

object SizeValidator extends ModelValidator[Size] {
  override def apply(size: Size)(implicit context: Option[Context] = None) = {
    Check(size)
      .field(_.width, "width")(isGreaterThan(0) and isSmallerOrEqual(5))
      .field(_.height, "height")(isGreaterThan(0) and isSmallerOrEqual(5))
      .validate
  }
}

val size = Size(0, 6)

SizeValidator(size) match {
  case Valid(s) =>
    println(s"valid $s")
  case iv: Invalid =>
    iv.errors.foreach(e => println(e.message))
}

// '0' must be greater than '0'.
// '6' must be smaller or equal to '5'.
~~~

##### Validate values wrapped in Option
~~~scala
optional(isPositive[Int])(Some(-1))
~~~

##### Validate elements in a List
~~~scala
listValidator(SizeValidator)(List(Size(0, 0), Size(1, 0)))
~~~

##### Validate keys or values in a Map
~~~scala
// pick the values
mapValidator[String, Size](tuple2Value(SizeValidator))(Map("one" -> Size(0, 0)))
// pick the keys
mapValidator[String, Size](tuple2Key(stringContains("one")))(Map("one" -> Size(0, 0)))
~~~

##### Recursive fields are supported
~~~scala
case class Item(name: String, items: List[Item])

object ItemValidator extends ModelValidator[Item] {
  override def apply(item: Item)(implicit parent: Option[Context]) = {
    Check(item)
      .field(_.name, "name")(stringNonEmpty)
      .field(_.items, "items")(listValidator(ItemValidator))
      .validate
  }
}

val item = Item("one", Item("two", Nil) :: Item("", Nil) :: Nil)

ItemValidator(item)
~~~

##### It's quick to create a custom Validator:
~~~scala
def stringContains(value: String) = Validator[String] { (s, context) =>
  if (s.contains(value)) Valid(s)
  else Invalid(Error(context, 'stringContains, Seq(s, value)))
}
~~~
Context is taken care of upstream, namely Check does the job when it is used to declare which fields of a class are to be validated.
So you just need to perform the check for validity. If the value is valid, return Valid with the value and if not, return Invalid. 
Invalid requires the context (from upstream), as well as an error message key and a Seq of values which are to be used in the error message.
Like ```String'{}' should contain '{}'.``` 


##### Validate fields whose validity depends on the values of each other
~~~scala
case class Person(name: String, age: Int, birthday: LocalDate)

def AgeValidator = Validator[(Int, LocalDate)] {
  case ((age, birthday), context) =>
    val shouldBeAge = Period.between(birthday, LocalDate.now()).getYears
    val isValid = shouldBeAge == age
    if (isValid) Valid(age -> birthday)
    else Invalid(Error(context, 'wrongAge, Seq(age, birthday, shouldBeAge)))
}

object PersonValidator extends ModelValidator[Person] {
  override def apply(person: Person)(implicit parent: Option[Context]) = {
    Check(person)
      .field(_.name, "name")(stringNonEmpty)
      .field(p => p.age -> p.birthday, "age")(AgeValidator)
      .validate
  }
}

PersonValidator(Person("max", 30, LocalDate.of(1950, 1, 1)))
~~~
