package com.peknight.codec.circe

import io.circe.{ACursor, DecodingFailure, Json}

trait Codec[A] extends io.circe.Codec[A] with Encoder[A] with Decoder[A]:
  def codec: com.peknight.codec.id.Codec[Json, ACursor, DecodingFailure, A]
  def encoder: com.peknight.codec.id.Encoder[Json, A] = codec
  def decoder: com.peknight.codec.id.Decoder[ACursor, DecodingFailure, A] = codec
end Codec
object Codec:
  private[this] case class CirceCodec[A](codec: com.peknight.codec.id.Codec[Json, ACursor, DecodingFailure, A])
    extends Codec[A]
  def apply[A](codec: com.peknight.codec.id.Codec[Json, ACursor, DecodingFailure, A]): Codec[A] = CirceCodec(codec)
end Codec
