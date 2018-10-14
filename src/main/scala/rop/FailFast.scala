package rop

import cats.{Id, ~>}
import cats.syntax.either._

class FailFast[E] extends (Railway[E, ?] ~> Either[E, ?]) {
	import Railway._

	def apply[A](r: Railway[E, A]): Either[E, A] = r match {
		
		case Success(a) => a.asRight[E]
		
		case Failure(e) => e.asLeft[A]
		
		case Ensure(onFailure, f, a) => a.asRight[E].ensure(onFailure)(f)
		
		case CatchNonFatal(onFailure, f, a) => Either.catchNonFatal(f(a)).leftMap(onFailure)
		
		case FromOption(onFailure, f, a) => Either.fromOption[E, A](f(a), onFailure)
	}
}
