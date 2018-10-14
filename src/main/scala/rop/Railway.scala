package rop

import cats.data.NonEmptyList

import scala.language.higherKinds

sealed trait Railway[A]

object Railway {
	case class Success[A](a: A) extends Railway[A]
	case class Failure[A](errors: NonEmptyList[String]) extends Railway[A]

	case class Ensure[A](filter: A => Boolean, onFailure: String, a: A) extends Railway[A]
	case class CatchNonFatal[A, B](onFailure: Throwable => String, f: A => B, a: A) extends Railway[B]
	case class FromOption[A, B](onFailure: String, f: A => Option[B], a: A) extends Railway[B]
}
