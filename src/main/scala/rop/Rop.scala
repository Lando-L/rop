package rop

import cats.data.NonEmptyList
import cats.free.Free

import scala.language.higherKinds

object Rop {
	import Railway._

	def succeed[A](a: A): Free[Railway, A] = Free.liftF(Success(a))
	def fail[A](error: String): Free[Railway, A] = Free.liftF(Failure(NonEmptyList(error, Nil)))

	def catchNonFatal[A, B](onFailure: Throwable => String)(f: A => B)(a: A): Free[Railway, B] = Free.liftF(CatchNonFatal(onFailure, f, a))
	def ensure[A](filter: A => Boolean)(onFailure: => String)(a: A): Free[Railway, A] = Free.liftF(Ensure(filter, onFailure, a))
	def fromOption[A, B](onFailure: => String)(f: A => Option[B])(a: A): Free[Railway, B] = Free.liftF(FromOption(onFailure, f, a))
}