package com.peknight.codec.instances.generic.decoder

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.generic.priority.MidPriority

import scala.reflect.ClassTag

trait DecoderValueInstances extends DecoderValueInstances1:

  given decodeNSM[F[_], S, A](
    using
    applicative: Applicative[F],
    numberType: NumberType[S],
    stringType: StringType[S],
    classTag: ClassTag[A],
    decoder: Decoder[F, Number, A],
  ): MidPriority[Decoder[F, Cursor[S], A]] =
    MidPriority(Decoder.decodeNS[F, S, A])
    
  given stringDecodeWithNumberDecoderM[F[_], A](using applicative:  Applicative[F], decoder: Decoder[F, Number, A])
  : MidPriority[Decoder[F, String, A]] =
    MidPriority(Decoder.stringDecodeWithNumberDecoder[F, A])
end DecoderValueInstances
object DecoderValueInstances extends DecoderValueInstances
