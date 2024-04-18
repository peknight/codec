package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.StringType
import com.peknight.codec.{Decoder, Encoder}

import scala.reflect.ClassTag

trait StringCodecInstances[T: ClassTag]:
  def toString(t: T): String
  def fromString(s: String): Either[DecodingFailure, T]
  given stringEncodeT[F[_]: Applicative]: Encoder[F, String, T] = Encoder.map[F, String, T](toString)
  given stringDecodeT[F[_]: Applicative]: Decoder[F, String, T] = Decoder.applicative[F, String, T](fromString)
  given encodeTS[F[_]: Applicative, S: StringType]: Encoder[F, S, T] = Encoder.encodeS[F, S, T]
  given decodeTS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], T] = Decoder.decodeS[F, S, T]
end StringCodecInstances

