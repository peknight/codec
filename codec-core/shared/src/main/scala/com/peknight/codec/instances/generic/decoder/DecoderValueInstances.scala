package com.peknight.codec.instances.generic.decoder

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.generic.priority.MidPriority

import scala.reflect.ClassTag

trait DecoderValueInstances extends DecoderValueInstances1:

  given numberDecoder[F[_], S, A](
    using
    applicative: Applicative[F],
    numberType: NumberType[S],
    stringType: StringType[S],
    classTag: ClassTag[A],
    numberDecoder: Decoder[F, Number, DecodingFailure, A],
    stringDecoder: Decoder[F, String, DecodingFailure, A]
  ): MidPriority[Decoder[F, Cursor[S], DecodingFailure, A]] =
    MidPriority(Decoder.numberDecoder[F, S, A])
end DecoderValueInstances
object DecoderValueInstances extends DecoderValueInstances