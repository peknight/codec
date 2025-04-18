package com.peknight.codec.instances.generic.decoder

import cats.{Applicative, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.generic.priority.MidPriority

import scala.reflect.ClassTag

trait DecoderValueInstances1:
  given decodeSM[F[_], S, A](
    using
    applicative: Applicative[F],
    stringType: StringType[S],
    decoder: Decoder[F, String, A],
    show: Show[S],
    classTag: ClassTag[A]
  ): MidPriority[Decoder[F, Cursor[S], A]] =
    MidPriority(Decoder.decodeS[F, S, A])

  given decodeNM[F[_], S, A](
    using
    applicative: Applicative[F],
    numberType: NumberType[S],
    decoder: Decoder[F, Number, A],
    show: Show[S],
    classTag: ClassTag[A]
  ): MidPriority[Decoder[F, Cursor[S], A]] =
    MidPriority(Decoder.decodeN[F, S, A])
end DecoderValueInstances1
