package rop

import cats.data.Kleisli

import scala.language.higherKinds

object RailwaySyntax {
	implicit class RailwaySwitch[F[_], T, A, B](func: A => F[B]) {
		def asRailway(implicit railway: Railway[F, T]): Kleisli[F, A, B] =
			railway.switch(func)
	}

	implicit class RailwayOneTrack[F[_], T, A, B](func: A => B) {
		def asRailway(onFailure: => T)(implicit railway: Railway[F, T]): Kleisli[F, A, B] =
			railway.oneTrack(onFailure)(func)
	}

	implicit class RailwayTwoTrack[F[_], T, A, B](func: F[A] => F[B]) {
		def asRailway(implicit railway: Railway[F, T]): Kleisli[F, A, B] =
			railway.twoTrack(func)
	}

	implicit class RailwayOneTrackTee[F[_], T, A](func: A => Unit) {
		def asRailway(onFailure: => T)(implicit railway: Railway[F, T]): Kleisli[F, A, A] =
			railway.oneTrackTee(onFailure)(func)
	}

	implicit class RailwayTwoTrackTee[F[_], T, A](func: F[A] => Unit) {
		def asRailway(implicit railway: Railway[F, T]): Kleisli[F, A, A] =
			railway.twoTrackTee(func)
	}
}
