package com.peknight.codec.circe

import cats.data.NonEmptyList
import com.peknight.codec.circe.instances.IsomorphismInstances.{cursorIsomorphism, decodingFailureIsomorphism, successCursorIsomorphism}
import io.circe.Decoder.{AccumulatingResult, Result}
import io.circe.{ACursor, HCursor, Json}

trait Decoder[A] extends io.circe.Decoder[A]:
  def decoder: com.peknight.codec.cursor.id.Decoder[Json, A]
  def apply(c: HCursor): Result[A] =
    decoder.decode(successCursorIsomorphism.from(c)).left.map(decodingFailureIsomorphism.to)
  override def tryDecode(c: ACursor): Result[A] =
    decoder.decode(cursorIsomorphism.from(c)).left.map(decodingFailureIsomorphism.to)
  override def decodeAccumulating(c: HCursor): AccumulatingResult[A] =
    decoder.decodeAccumulating(successCursorIsomorphism.from(c)).leftMap(_.map(decodingFailureIsomorphism.to))
  override def tryDecodeAccumulating(c: ACursor): AccumulatingResult[A] =
    decoder.decodeAccumulating(cursorIsomorphism.from(c)).leftMap(_.map(decodingFailureIsomorphism.to))
end Decoder
object Decoder:
  private[this] case class CirceDecoder[A](decoder: com.peknight.codec.cursor.id.Decoder[Json, A]) extends Decoder[A]
  def apply[A](decoder: com.peknight.codec.cursor.id.Decoder[Json, A]): Decoder[A] = CirceDecoder(decoder)
end Decoder
