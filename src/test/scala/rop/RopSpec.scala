package rop

import cats.free.Free
import cats.instances.either._
import org.scalatest.{FlatSpec, Matchers}

class RopSpec extends FlatSpec with Matchers {

	"rop" should "be fancy" in {

		type FormData = Map[String, String]
		type Validated[A] = Free[Railway, A]

		def getValue(field: String): FormData => Validated[String] =
			Rop.fromOption[FormData, String](s"Field $field is not defined")(_.get(field))

		def parseInt(field: String): String => Validated[Int] =
			Rop.catchNonFatal[String, Int](_ => s"Field $field is not a number")(_.toInt)

		def nonBlank(field: String): String => Validated[String] =
			Rop.ensure[String](_.nonEmpty)(s"Field $field is empty")

		def nonNegative(field: String): Int => Validated[Int] =
			Rop.ensure[Int](_ > 0)(s"Field $field is negative")

		def readName(field: String): FormData => Validated[String] =
			getValue(field) andThen (_.flatMap(nonBlank(field)))

		def readAge(field: String): FormData => Validated[Int] =
			getValue(field) andThen (_.flatMap(parseInt(field)).flatMap(nonNegative(field)))

		def app(data: FormData): Validated[String] = {
			for {
				name <- readName("name")(data)
				age <- readAge("age")(data)
				greetings = s"Hello $name, $age"
			} yield greetings
		}

		println(app(Map("name" -> "Lando", "age" -> "23")).foldMap(FailFast))
	}
}
