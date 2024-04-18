package com.peknight.codec.instances.generic.decoder

import cats.Functor
import com.peknight.codec.{Decoder, Encoder}
import com.peknight.generic.priority.MidPriority

trait DecoderEncoderInstances:
  given decodeWithEncoderM[F[_], T, A](using functor: Functor[F], encoder: Encoder[F, A, T])
  : MidPriority[Decoder[F, T, A]] =
    MidPriority(Decoder.decodeWithEncoder[F, T, A])
  end decodeWithEncoderM
end DecoderEncoderInstances
object DecoderEncoderInstances extends DecoderEncoderInstances
