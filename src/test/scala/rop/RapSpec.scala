package rop

import Rap._

import cats.data.NonEmptyList
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.apply._

import org.scalatest.{FlatSpec, Matchers}

class RapSpec extends FlatSpec with Matchers {
	"railway" should "be fancy" in {
		type FormData = Map[String, String]
		type Errors = NonEmptyList[String]
		type Result[A] = Either[Errors, A]

		case class User(email: String, password: String)

		def getValue(name: String): Rap[Result, FormData, String] =
			Switch[Result, FormData, String](_.get(name).toRight(NonEmptyList(s"$name is not defined", Nil)))

		def nonBlank(name: String): Rap[Result, String, String] =
			Switch[Result, String, String](_.asRight[Errors].ensure(NonEmptyList(s"$name cannot be empty", Nil))(_.nonEmpty))

		def minLength(name: String)(length: Int): Rap[Result, String, String] =
			Switch[Result, String, String](_.asRight[Errors].ensure(NonEmptyList(s"$name is too short", Nil))(_.length >= length))

		def cleanEmail: Rap[Result, String, String] =
			OneTrack[Result, String, String](_.trim.toLowerCase)

		def log: Rap[Result, User, User] =
			TwoTrack[Result, User, User] {
				case Left(errors) => errors.toList.foreach(println); Left(errors)
				case Right(user) => println(s"Welcome back ${user.email}"); Right(user)
			}

		val readEmail: Rap[Result, FormData, String] =
			getValue("email") andThen nonBlank("email") andThen cleanEmail

		val readPassword: Rap[Result, FormData, String] =
			getValue("password") andThen minLength("password")(6)

		val readUser: Rap[Result, FormData, User] =
			Switch[Result, FormData, User] { data =>
				(
					readEmail.run(data).toValidated,
					readPassword.run(data).toValidated
				).mapN(User.apply _).toEither
			}

		def login(data: FormData): Unit =
			(readUser andThen log).run(data)

		// Examples

		val validLogin = Map("email" -> "test@example.com", "password" -> "123456")
		val invalidLogin = Map("password" -> "12345")

		login(validLogin)
		login(invalidLogin)
	}
}
