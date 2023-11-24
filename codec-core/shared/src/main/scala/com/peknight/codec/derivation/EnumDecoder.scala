package com.peknight.codec.derivation

trait EnumDecoder[F[_], T, E, A] extends SumDecoder[F, T, E, A]
