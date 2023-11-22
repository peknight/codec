package com.peknight.codec.derivation

import com.peknight.codec.Encoder

trait SumEncoder[F[_], S, A] extends Encoder[F, S, A]
