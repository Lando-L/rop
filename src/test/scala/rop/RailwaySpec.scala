package rop

import rop.RailwayInstances._
import rop.RailwaySyntax._

import cats.data.{Kleisli, NonEmptyList}
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.apply._

import org.scalatest.{FlatSpec, Matchers}

class RailwaySpec extends FlatSpec with Matchers {
	"eitherRailway" should "do something" in {

		sealed trait ValidationError

		case class FieldNotDefined(field: String) extends ValidationError {
			override def toString: String = s"$field is not defined"
		}

		case class FieldNaN(field: String) extends ValidationError {
			override def toString: String = s"$field is not a number"
		}

		case class FieldEmpty(field: String) extends ValidationError {
			override def toString: String = s"$field cannot be empty"
		}

		case class FieldNegativeInteger(field: String) extends ValidationError {
			override def toString: String = s"$field cannot be negative"
		}
		case object UnknownError extends ValidationError {
			override def toString: String = "Unknown error"
		}

		type FormData = Map[String, String]
		type Error = NonEmptyList[ValidationError]
		type FailFast[A] = Either[Error, A]
		type Result[A, B] = Kleisli[FailFast, A, B]

		case class User(name: String, age: Int)

		// Validation Functions

		def getValue(name: String): Result[FormData, String] =
			Railway.fromOneTrack[FailFast, Error, FormData, String](NonEmptyList(FieldNotDefined(name), Nil))(data => data(name))

		def parseInt(name: String): Result[String, Int] =
			Railway.fromOneTrack[FailFast, Error, String, Int](NonEmptyList(FieldNaN(name), Nil))(_.toInt)

		def nonBlank(name: String): Result[String, String] =
			Railway.fromSwitch[FailFast, Error, String, String](_.asRight[Error].ensure(NonEmptyList(FieldEmpty(name), Nil))(_.nonEmpty))

		def nonNegative(name: String): Result[Int, Int] =
			Railway.fromSwitch[FailFast, Error, Int, Int](_.asRight[Error].ensure(NonEmptyList(FieldNegativeInteger(name), Nil))(_ >= 0))

		def greetUser: Result[User, User] =
			Railway.fromTee[FailFast, Error, User](NonEmptyList(UnknownError, Nil))(user => println(s"Hello ${user.name}! How are you?"))

		// Putting everything together using Railway

		val readName: Result[FormData, String] =
			getValue("name") andThen nonBlank("name")

		val readAge: Result[FormData, Int] =
			getValue("age") andThen nonBlank("age") andThen parseInt("age") andThen nonNegative("age")

		def readUser: Result[FormData, User] =
			Railway.fromSwitch[FailFast, Error, FormData, User]{ data =>
				val name = readName.run(data).toValidated
				val age = readAge.run(data).toValidated
				(name, age).mapN(User.apply).toEither
			}

		def allTogether(data: FormData): FailFast[User] =
			(readUser andThen greetUser).run(data)

		// Example Users

		val thomas = Map("name" -> "Thomas Smith", "age" -> "65")
		val noName = Map("age" -> "18")
		val negativeAge = Map("name" -> "Mac Miller", "age" -> "-19")
		val everything = Map("name" -> "", "age" -> "pimmel")

		println(allTogether(thomas))
		println(allTogether(noName))
		println(allTogether(negativeAge))
		println(allTogether(everything))
	}
}
