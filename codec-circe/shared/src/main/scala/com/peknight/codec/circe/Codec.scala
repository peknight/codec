package com.peknight.codec.circe

import io.circe.Json

trait Codec[A] extends io.circe.Codec[A] with Encoder[A] with Decoder[A]:
  def codec: com.peknight.codec.cursor.id.Codec[Json, A]
  def encoder: com.peknight.codec.id.Encoder[Json, A] = codec
  def decoder: com.peknight.codec.cursor.id.Decoder[Json, A] = codec
end Codec
object Codec:
  private[this] case class CirceCodec[A](codec: com.peknight.codec.cursor.id.Codec[Json, A]) extends Codec[A]
  def apply[A](codec: com.peknight.codec.cursor.id.Codec[Json, A]): Codec[A] = CirceCodec(codec)
end Codec
