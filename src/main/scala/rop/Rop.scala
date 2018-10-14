package rop

import cats.free.Free

import scala.language.higherKinds

object Rop {
	import Railway._

	def succeed[E, A](a: A): Free[Railway[E, ?], A] = Free.liftF(Success(a))
	def fail[E, A](error: E): Free[Railway[E, ?], A] = Free.liftF(Failure(error))

	def catchNonFatal[E, A, B](onFailure: Throwable => E)(f: A => B)(a: A): Free[Railway[E, ?], B] = Free.liftF(CatchNonFatal(onFailure, f, a))
	def ensure[E, A](onFailure: => E)(f: A => Boolean)(a: A): Free[Railway[E, ?], A] = Free.liftF(Ensure(onFailure, f, a))
	def fromOption[E, A, B](onFailure: => E)(f: A => Option[B])(a: A): Free[Railway[E, ?], B] = Free.liftF(FromOption(onFailure, f, a))
}