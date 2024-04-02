package com.peknight.codec.instances.generic.decoder

import cats.Functor
import com.peknight.codec.{Decoder, Encoder}
import com.peknight.generic.priority.MidPriority

trait DecoderEncoderInstances:
  given encoderDecoder[F[_], T, E, A] (using functor: Functor[F], encoder: Encoder[F, A, T])
  : MidPriority[Decoder[F, T, E, A]] =
    MidPriority(Decoder.encoderDecoder[F, T, E, A](encoder))
  end encoderDecoder
end DecoderEncoderInstances
object DecoderEncoderInstances extends DecoderEncoderInstances
