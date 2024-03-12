package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.StringType

import scala.reflect.ClassTag

trait DecoderStringInstances1:
  given stringDecoder[F[_], S, A](
    using
    applicative: Applicative[F],
    stringType: StringType[S],
    classTag: ClassTag[A],
    decoder: Decoder[F, String, DecodingFailure, A]
  ): Decoder[F, Cursor[S], DecodingFailure, A] =
    Decoder.stringDecoder[F, S, A](decoder)
end DecoderStringInstances1
