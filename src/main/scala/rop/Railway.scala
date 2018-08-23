package rop

import cats.data.Kleisli

import scala.language.higherKinds

trait Railway[F[_], T] {
	def switch[A, B](track: A => F[B]): Kleisli[F, A, B] =
		Kleisli[F, A, B](track)

	def oneTrack[A, B](onFailure: => T)(track: A => B): Kleisli[F, A, B]
	def twoTrack[A, B](track: F[A] => F[B]): Kleisli[F, A, B]
	def tee[A](onFailure: => T)(track: A => Unit): Kleisli[F, A, A]
}

object Railway {
	def fromSwitch[F[_], T, A, B](func: A => F[B])(implicit railway: Railway[F, T]): Kleisli[F, A, B] =
		railway.switch(func)

	def fromOneTrack[F[_], T, A, B](onFailure: => T)(func: A => B)(implicit railway: Railway[F, T]): Kleisli[F, A, B] =
		railway.oneTrack(onFailure)(func)

	def fromTwoTrack[F[_], T, A, B](func: F[A] => F[B])(implicit railway: Railway[F, T]): Kleisli[F, A, B] =
		railway.twoTrack(func)

	def fromTee[F[_], T, A](onFailure: => T)(func: A => Unit)(implicit railway: Railway[F, T]): Kleisli[F, A, A] =
		railway.tee(onFailure)(func)
}
