package rop

import scala.language.higherKinds

sealed trait Railway[E, A]

object Railway {
	case class Success[E, A](a: A) extends Railway[E, A]
	case class Failure[E, A](error: E) extends Railway[E, A]

	case class Ensure[E, A](onFailure: E, f: A => Boolean, a: A) extends Railway[E, A]
	case class CatchNonFatal[E, A, B](onFailure: Throwable => E, f: A => B, a: A) extends Railway[E, B]
	case class FromOption[E, A, B](onFailure: E, f: A => Option[B], a: A) extends Railway[E, B]
}
