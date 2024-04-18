package com.peknight.codec.circe

import cats.Id
import com.peknight.codec.cursor.Cursor
import io.circe.Json

trait Codec[A] extends io.circe.Codec[A] with Encoder[A] with Decoder[A]:
  def codec: com.peknight.codec.Codec[Id, Json, Cursor[Json], A]
  def encoder: com.peknight.codec.Encoder[Id, Json, A] = codec
  def decoder: com.peknight.codec.Decoder[Id, Cursor[Json], A] = codec
end Codec
object Codec:
  private[this] case class CirceCodec[A](codec: com.peknight.codec.Codec[Id, Json, Cursor[Json], A])
    extends Codec[A]
  def apply[A](codec: com.peknight.codec.Codec[Id, Json, Cursor[Json], A]): Codec[A] = CirceCodec(codec)
end Codec
