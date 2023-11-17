package com.peknight.codec.derivation

import cats.Functor
import com.peknight.codec.Encoder

trait SumEncoder[F[_], S, A] extends Encoder[F, S, A]
