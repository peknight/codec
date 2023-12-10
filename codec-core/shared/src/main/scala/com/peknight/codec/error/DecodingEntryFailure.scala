package com.peknight.codec.error

import cats.data.NonEmptyList
import cats.kernel.{Monoid, Semigroup}
import cats.syntax.semigroup.*

case class DecodingEntryFailure[A, S](value: A, keyFailure: DecodingFailure[String], valueFailure: DecodingFailure[S])
  extends DecodingFailure[A]:
  def map[B](f: A => B): DecodingFailure[B] = DecodingEntryFailure(f(value), keyFailure, valueFailure)
end DecodingEntryFailure
