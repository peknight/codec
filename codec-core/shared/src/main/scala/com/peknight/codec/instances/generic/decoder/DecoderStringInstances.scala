package com.peknight.codec.instances.generic.decoder

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.StringType
import com.peknight.generic.priority.MidPriority

import scala.reflect.ClassTag

trait DecoderStringInstances:
  given stringDecoder[F[_], S, A](
    using
    applicative: Applicative[F],
    stringType: StringType[S],
    classTag: ClassTag[A],
    decoder: Decoder[F, String, DecodingFailure, A]
  ): MidPriority[Decoder[F, Cursor[S], DecodingFailure, A]] =
    MidPriority(Decoder.stringDecoder[F, S, A])
end DecoderStringInstances
object DecoderStringInstances extends DecoderStringInstances
