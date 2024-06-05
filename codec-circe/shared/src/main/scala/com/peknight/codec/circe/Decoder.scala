package com.peknight.codec.circe

import cats.Id
import cats.syntax.either.*
import com.peknight.codec.circe.iso.{cursorIsomorphism, decodingFailureIsomorphism, successCursorIsomorphism}
import com.peknight.codec.cursor.Cursor
import io.circe.Decoder.{AccumulatingResult, Result}
import io.circe.{ACursor, HCursor, Json}

trait Decoder[A] extends io.circe.Decoder[A]:
  def decoder: com.peknight.codec.Decoder[Id, Cursor[Json], A]
  def apply(c: HCursor): Result[A] =
    decoder.decode(successCursorIsomorphism[Id].from(c)).left.map(decodingFailureIsomorphism[Id].to)
  override def tryDecode(c: ACursor): Result[A] =
    decoder.decode(cursorIsomorphism[Id].from(c)).left.map(decodingFailureIsomorphism[Id].to)
  override def decodeAccumulating(c: HCursor): AccumulatingResult[A] =
    decoder.decode(successCursorIsomorphism[Id].from(c)).toValidatedNel.leftMap(_.map(decodingFailureIsomorphism[Id].to))
  override def tryDecodeAccumulating(c: ACursor): AccumulatingResult[A] =
    decoder.decode(cursorIsomorphism[Id].from(c)).toValidatedNel.leftMap(_.map(decodingFailureIsomorphism[Id].to))
end Decoder
object Decoder:
  private case class CirceDecoder[A](decoder: com.peknight.codec.Decoder[Id, Cursor[Json], A]) extends Decoder[A]
  def apply[A](decoder: com.peknight.codec.Decoder[Id, Cursor[Json], A]): Decoder[A] = CirceDecoder(decoder)
end Decoder
