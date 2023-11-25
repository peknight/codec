package com.peknight.codec.circe

import io.circe.Decoder.{AccumulatingResult, Result}
import io.circe.{ACursor, DecodingFailure, HCursor}

trait Decoder[A] extends io.circe.Decoder[A]:
  def decoder: com.peknight.codec.id.Decoder[ACursor, DecodingFailure, A]
  def apply(c: HCursor): Result[A] = decoder.decode(c)
  override def tryDecode(c: ACursor): Result[A] = decoder.decode(c)
  override def decodeAccumulating(c: HCursor): AccumulatingResult[A] = decoder.decodeAccumulating(c)
  override def tryDecodeAccumulating(c: ACursor): AccumulatingResult[A] = decoder.decodeAccumulating(c)
end Decoder
object Decoder:
  private[this] case class CirceDecoder[A](decoder: com.peknight.codec.id.Decoder[ACursor, DecodingFailure, A])
    extends Decoder[A]
  def apply[A](decoder: com.peknight.codec.id.Decoder[ACursor, DecodingFailure, A]): Decoder[A] = CirceDecoder(decoder)
end Decoder
