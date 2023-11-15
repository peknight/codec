package com.peknight.codec.derivation

import cats.Functor
import com.peknight.codec.Encoder

trait SumEncoder[F[_]: Functor, S, A] extends Encoder.AsObject[F, S, A]
object SumEncoder:
  type Aux[F[_], S, O, A] = SumEncoder[F, S, A] { type Object = O }
end SumEncoder
