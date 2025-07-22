package com.peknight.codec.http4s.instances

import cats.{Applicative, Show}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.StringType
import com.peknight.codec.{Codec, Decoder, Encoder}
import org.http4s.Header

import scala.reflect.ClassTag

trait HeaderInstances:
  given stringCodecHeader[F[_], A, T <: Header.Type](using applicative: Applicative[F], header: Header[A, T])
  : Codec[F, String, String, A] =
    Codec.applicative[F, String, String, A](header.value)(t => header.parse(t).left.map(DecodingFailure.apply))

  given encodeHeaderS[F[_], S, A, T <: Header.Type](using
    applicative: Applicative[F], stringType: StringType[S], header: Header[A, T]
  ): Encoder[F, S, A] =
    Encoder.encodeS[F, S, A]

  given decodeHeaderS[F[_], S, A, T <: Header.Type](using
    applicative: Applicative[F], stringType: StringType[S], show: Show[S], classTag: ClassTag[A], header: Header[A, T]
  ): Decoder[F, Cursor[S], A] =
    Decoder.decodeS[F, S, A]
end HeaderInstances
object HeaderInstances extends HeaderInstances
