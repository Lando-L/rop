package rop

import cats.data.Kleisli
import cats.syntax.either._

import scala.util.Try

object RailwayInstances {
	implicit val tryRailway: Railway[Try, Throwable] =
		new Railway[Try, Throwable] {
			override def oneTrack[A, B](onFailure: => Throwable)(track: A => B): Kleisli[Try, A, B] =
				Kleisli[Try, A, B](a => Try(track(a)))

			override def twoTrack[A, B](track: Try[A] => Try[B]): Kleisli[Try, A, B] =
				Kleisli[Try, A, B](a => track(Try(a)))

			override def oneTrackTee[A](onFailure: => Throwable)(track: A => Unit): Kleisli[Try, A, A] =
				Kleisli[Try, A, A] { a =>
					Try {
						track(a)
						a
					}
				}

			override def twoTrackTee[A](track: Try[A] => Unit): Kleisli[Try, A, A] =
				Kleisli[Try, A, A] { a =>
					track(Try(a))
					Try(a)
				}
		}

	implicit def eitherRailway[T]: Railway[Either[T, ?], T] =
		new Railway[Either[T, ?], T] {
			override def oneTrack[A, B](onFailure: => T)(track: A => B): Kleisli[Either[T, ?], A, B] =
				Kleisli[Either[T, ?], A, B](a => Either.catchNonFatal(track(a)).leftMap(_ => onFailure))

			override def twoTrack[A, B](track: Either[T, A] => Either[T, B]): Kleisli[Either[T, ?], A, B] =
				Kleisli[Either[T, ?], A, B](track compose Right.apply)

			override def oneTrackTee[A](onFailure: => T)(track: A => Unit): Kleisli[Either[T, ?], A, A] =
				Kleisli[Either[T, ?], A, A] { a =>
					Either.catchNonFatal {
						track(a)
						a
					}.leftMap(_=> onFailure)
				}

			override def twoTrackTee[A](track: Either[T, A] => Unit): Kleisli[Either[T, ?], A, A] =
				Kleisli[Either[T, ?], A, A] { a =>
					track(Right(a))
					Right(a)
				}
		}

	implicit val optionRailway: Railway[Option, None.type ] =
		new Railway[Option, None.type] {
			override def oneTrack[A, B](onFailure: => None.type )(track: A => B): Kleisli[Option, A, B] =
				Kleisli[Option, A, B] { a =>
					try {
						Some(track(a))
					} catch { case _: Exception => None }
				}

			override def twoTrack[A, B](track: Option[A] => Option[B]): Kleisli[Option, A, B] =
				Kleisli[Option, A, B](track compose Option.apply)

			override def oneTrackTee[A](onFailure: => None.type)(track: A => Unit): Kleisli[Option, A, A] =
				Kleisli[Option, A, A] { a =>
					try {
						track(a)
						Some(a)
					} catch { case _: Exception => None }
				}

			override def twoTrackTee[A](track: Option[A] => Unit): Kleisli[Option, A, A] =
				Kleisli[Option, A, A] { a =>
					track(Some(a))
					Some(a)
				}
		}
}
