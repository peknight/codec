package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Decoder, Encoder}

import scala.reflect.ClassTag

trait NumberCodecInstances1[T: ClassTag]:
  def toNumber(t: T): Number
  def fromNumber(n: Number): Option[T]

  given numberDecodeT[F[_] : Applicative]: Decoder[F, Number, T] =
    Decoder.numberDecodeNumberOption[F, T](fromNumber)

  given decodeTN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], T] = Decoder.decodeN[F, S, T]

  given stringEncodeT[F[_]: Applicative]: Encoder[F, String, T] =
    Encoder.applicative[F, String, T](a => toNumber(a).toString)
  given stringDecodeT[F[_]: Applicative]: Decoder[F, String, T] = Decoder.stringDecodeWithNumberDecoder[F, T]

  given encodeTS[F[_]: Applicative, S: StringType]: Encoder[F, S, T] = Encoder.encodeS[F, S, T]
  given decodeTS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], T] = Decoder.decodeS[F, S, T]

end NumberCodecInstances1
