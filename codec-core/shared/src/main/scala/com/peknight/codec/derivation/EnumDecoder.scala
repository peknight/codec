package com.peknight.codec.derivation

trait EnumDecoder[F[_], T, A] extends SumDecoder[F, T, A]
