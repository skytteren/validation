package validation

import munit.FunSuite
import validation.Error.{Composite, Single}
import validation.Result.{Invalid, Valid}

class ValidationTest extends FunSuite:

  case class Poststed(value: String)
  case class Postnummer(value: Int)
  object Postnummer {
    given Constraint[Postnummer] =
      Constraint.check[Postnummer](_.value >= 0, n => s"Postnummer er negativt (${n.value}), men må være positivt")
  }

  case class Adresse(adresse: String, postnummer: Postnummer, poststed: Poststed)

  case class Person(navn: String, adresse: Adresse)

  val adresse = Adresse(
    adresse = "Svingen 12",
    postnummer = Postnummer(1234),
    poststed = Poststed("Stedet")
  )

  val person: Person = Person(
    navn = "Navet",
    adresse = adresse
  )

  val invalidAdresse: Adresse = Adresse(
    adresse = "Svingen 12",
    postnummer = Postnummer(-1),
    poststed = Poststed("Stedet")
  )
  val invalidPerson: Person = Person(
    navn = "Navet",
    adresse = invalidAdresse
  )

  test("int validation"):
    val result = Validation(4)
    assertEquals(result, Valid(4))

  test("error validation"):
    val result = Validation(Postnummer(-1))
    assertEquals(result, Invalid(Error.Single("Postnummer er negativt (-1), men må være positivt", Nil)))

  test("string validation"):
    val result = Validation("String")
    assertEquals(result, Valid("String"))

  test("product validation"):
    val result: Result[Adresse] = Validation(adresse)
    assertEquals(result, Valid(adresse))

  test("nested validation"):
    val result = Validation(person)
    assertEquals(result, Valid(person))

  test("nested validation with error"):
    val result = Validation(invalidPerson)
    assertEquals(
      result,
      Invalid(
        Error.Composite(
          message = "",
          path = Nil,
          errors = List(
            Error.Composite(
              message = "",
              path = List("adresse"),
              errors = List(
                Error.Single("Postnummer er negativt (-1), men må være positivt", List("adresse", "postnummer"))
              )
            )
          )
        )
      )
    )

  test("Validate tuple"):
    val pair = ("", 12)
    val res: Result[(String, Int)] = Validation(pair)

    assertEquals(res, Valid("", 12))

  test("Invalidate tuple"):
    val pair = ("", Postnummer(-1))
    val res: Result[(String, Postnummer)] = Validation(pair)

    assertEquals(
      res,
      Invalid(
        error = Composite(
          message = "",
          path = Nil,
          errors = List(
            Single(
              message = "Postnummer er negativt (-1), men må være positivt",
              path = List(
                "_2"
              )
            )
          )
        )
      )
    )

  test("map Result[tuple] "):
    case class Pair(st: String, i: Int)
    val res: Result[Pair] = Result.Valid("", 12).map(Pair.apply)

    assertEquals(res, Valid(Pair("", 12)))

  test("Valid toEither"):
    val result = Validation(4).toEither
    assertEquals(result, Right(4))

  test("Invalid toEither"):
    val result = Validation(Postnummer(-1)).toEither
    assertEquals(result, Left(Error.Single("Postnummer er negativt (-1), men må være positivt", Nil)))

  test("Validate coMap constraint"):
    case class Pair(a: String, b: String)
    given Constraint[Pair] = Constraint[(String, String)].coMap[Pair](pair => (pair.b, pair.b))
    val res: Result[Pair] = Validation(Pair("", ""))

    assertEquals(res, Valid(Pair("", "")))

  test("Validate NamedTuple"):
    type Pair = (a: String, b: String)
    val res: Result[Pair] = Validation((a = "", b = ""))

    assertEquals(res, Valid((a = "", b = "")))

  test("Validate NamedTuple with constraints"):
    type Pair = (a: String, b: String)
    given Constraint[Pair] = Constraint[(String, String)].coMap[Pair](pair => (pair.b, pair.b))
    val res: Result[Pair] = Validation((a = "", b = ""))

    assertEquals(res, Valid((a = "", b = "")))

  test("Validate list"):
    val list = List(1, 12)
    val res: Result[List[Int]] = Validation(list)

    assertEquals(res, Valid(list))

  test("Invalidate list"):
    val seq = Seq(Postnummer(12), Postnummer(-1))
    val res: Result[Seq[Postnummer]] = Validation(seq)

    assertEquals(
      res,
      Invalid(
        error = Composite(
          message = "",
          path = Nil,
          errors = List(
            Single(
              message = "Postnummer er negativt (-1), men må være positivt",
              path = List(
                "1"
              )
            )
          )
        )
      )
    )

  test("Validate Map"):
    val map = Map("one" -> 1, "two" -> 12)
    val res: Result[Map[String, Int]] = Validation(map)

    assertEquals(res, Valid(map))

  test("Invalidate Map"):
    val seq = Map("one" -> Postnummer(12), "two" -> Postnummer(-1))
    val res: Result[Map[String, Postnummer]] = Validation(seq)

    assertEquals(
      res,
      Invalid(
        error = Composite(
          message = "",
          path = Nil,
          errors = List(
            Single(
              message = "Postnummer er negativt (-1), men må være positivt",
              path = List(
                "two"
              )
            )
          )
        )
      )
    )

  test("Validate Enum"):
    enum FooBar:
      case Foo, Bar
    val fooBar: FooBar = FooBar.Foo

    val res: Result[FooBar] = Validation(fooBar)

    assertEquals(res, Valid(fooBar))

  test("Inalidate Enum"):
    enum FooBar:
      case Foo(postnummer: Postnummer)
      case Bar
    val fooBar: FooBar = FooBar.Foo(Postnummer(-1))

    val res: Result[FooBar] = Validation(fooBar)

    assertEquals(
      res,
      Invalid(
        error = Composite(
          message = "",
          path = Nil,
          errors = List(
            Single(
              message = "Postnummer er negativt (-1), men må være positivt",
              path = List(
                "postnummer"
              )
            )
          )
        )
      )
    )
