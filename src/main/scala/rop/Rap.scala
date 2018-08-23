package rop

import cats.Monad

import scala.language.higherKinds

trait Rap[F[_], A, B] {
	def run(a: A)(implicit monad: Monad[F]): F[B]
	def andThen[C](that: Rap[F, B, C])(implicit monad: Monad[F]): Rap[F, A, C]
}

object Rap {
	final case class Switch[F[_], A, B](track: A => F[B]) extends Rap[F, A, B] {
		override def andThen[C](that: Rap[F, B, C])(implicit monad: Monad[F]): Rap[F, A, C] = that match {
			case Switch(otherTrack) => Switch[F, A, C](track andThen (monad.flatMap(_)(otherTrack)))
			case OneTrack(otherTrack) => Switch[F, A, C](track andThen monad.lift(otherTrack))
			case TwoTrack(otherTrack) => Switch[F, A, C](track andThen otherTrack)
			case oneTrackTee: Tee[F, C] => Switch[F, A, C](track andThen (monad.flatMap(_)(oneTrackTee.run)))
		}

		override def run(a: A)(implicit monad: Monad[F]): F[B] = track(a)
	}

	final case class OneTrack[F[_], A, B](track: A => B) extends Rap[F, A, B] {
		override def andThen[C](that: Rap[F, B, C])(implicit monad: Monad[F]): Rap[F, A, C] = that match {
			case Switch(otherTrack) => Switch[F, A, C](track andThen otherTrack)
			case OneTrack(otherTrack) => OneTrack[F, A, C](track andThen otherTrack)
			case TwoTrack(otherTrack) => Switch[F, A, C](run _ andThen otherTrack)
			case oneTrackTee: Tee[F, C] => Switch[F, A, C](track andThen oneTrackTee.run)
		}

		override def run(a: A)(implicit monad: Monad[F]): F[B] = monad.pure(track(a))
	}

	final case class TwoTrack[F[_], A, B](track: F[A] => F[B]) extends Rap[F, A, B] {
		override def andThen[C](that: Rap[F, B, C])(implicit monad: Monad[F]): Rap[F, A, C] = that match {
			case Switch(otherTrack) => TwoTrack[F, A, C](track andThen (monad.flatMap(_)(otherTrack)))
			case OneTrack(otherTrack) => TwoTrack[F, A, C](track andThen monad.lift(otherTrack))
			case TwoTrack(otherTrack) => TwoTrack[F, A, C](track andThen otherTrack)
			case oneTrackTee: Tee[F, C] => TwoTrack[F, A, C](track andThen (monad.flatMap(_)(oneTrackTee.run)))
		}

		override def run(a: A)(implicit monad: Monad[F]): F[B] = track(monad.pure(a))
	}

	final case class Tee[F[_], A](track: A => Unit) extends Rap[F, A, A] {
		override def andThen[C](that: Rap[F, A, C])(implicit monad: Monad[F]): Rap[F, A, C] = that match {
			case Switch(otherTrack) => Switch[F, A, C](run _ andThen (monad.flatMap(_)(otherTrack)))
			case OneTrack(otherTrack) => Switch[F, A, C](run _ andThen monad.lift(otherTrack))
			case TwoTrack(otherTrack) => Switch[F, A, C](run _ andThen otherTrack)
			case oneTrackTee: Tee[F, C] => Switch[F, A, C](run _ andThen (monad.flatMap(_)(oneTrackTee.run)))
		}

		override def run(a: A)(implicit monad: Monad[F]): F[A] = {
			track(a)
			monad.pure(a)
		}
	}
}
