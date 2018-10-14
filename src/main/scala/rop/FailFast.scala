package rop

import cats.{Id, ~>}
import cats.data.NonEmptyList
import cats.syntax.either._

object FailFast extends (Railway ~> Either[NonEmptyList[String], ?]) {
	import Railway._

	def apply[A](r: Railway[A]): Either[NonEmptyList[String], A] = r match {
		case Success(a) => a.asRight[NonEmptyList[String]]
		case Failure(e) => e.asLeft[A]
		case Ensure(filter, onFailure, a) => a.asRight[String].ensure(onFailure)(filter).leftMap(t => NonEmptyList(t, Nil))
		case CatchNonFatal(onFailure, f, a) => Either.catchNonFatal(f(a)).leftMap(t => NonEmptyList(onFailure(t), Nil))
		case FromOption(onFailure, f, a) => Either.fromOption[NonEmptyList[String], A](f(a), NonEmptyList(onFailure, Nil))
	}
}
