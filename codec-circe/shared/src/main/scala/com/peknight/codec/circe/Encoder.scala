package com.peknight.codec.circe

import cats.Id
import io.circe.Json

trait Encoder[A] extends io.circe.Encoder[A]:
  def encoder: com.peknight.codec.Encoder[Id, Json, A]
  def apply(a: A): Json = encoder.encode(a)
end Encoder
object Encoder:
  private[this] case class CirceEncoder[A](encoder: com.peknight.codec.Encoder[Id, Json, A]) extends Encoder[A]
  def apply[A](encoder: com.peknight.codec.Encoder[Id, Json, A]): Encoder[A] = CirceEncoder(encoder)
end Encoder

