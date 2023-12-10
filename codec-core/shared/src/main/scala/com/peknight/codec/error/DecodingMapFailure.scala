package com.peknight.codec.error

import cats.data.NonEmptyList
import cats.kernel.Semigroup
import cats.syntax.semigroup.*

case class DecodingMapFailure[A, S](value: A, errors: NonEmptyList[DecodingFailure[(String, S)]])
  extends DecodingFailure[A]:
  def map[B](f: A => B): DecodingFailure[B] = DecodingMapFailure(f(value), errors)
end DecodingMapFailure
object DecodingMapFailure:
  given [A, S](using semigroup: Semigroup[A]): Semigroup[DecodingMapFailure[A, S]] with
    def combine(x: DecodingMapFailure[A, S], y: DecodingMapFailure[A, S]): DecodingMapFailure[A, S] =
      DecodingMapFailure(x.value |+| y.value, x.errors |+| y.errors)
  end given
end DecodingMapFailure
