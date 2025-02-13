package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Decoder, Encoder}

import scala.reflect.ClassTag

trait NumberCodecInstances[T: ClassTag] extends NumberCodecInstances1[T]:
  given numberEncodeT[F[_]: Applicative]: Encoder[F, Number, T] =
    Encoder.map[F, Number, T](toNumber)
  given encodeTN[F[_]: Applicative, S: NumberType]: Encoder[F, S, T] = Encoder.encodeN[F, S, T]
  given decodeTNS[F[_]: Applicative, S: {NumberType, StringType}]: Decoder[F, Cursor[S], T] = Decoder.decodeNS[F, S, T]
end NumberCodecInstances
