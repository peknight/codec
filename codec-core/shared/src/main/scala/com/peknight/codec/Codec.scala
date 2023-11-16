package com.peknight.codec

import cats.Functor
import com.peknight.codec.derivation.CodecDerivation

trait Codec[F[_], S, T, E, A] extends Encoder[F, S, A] with Decoder[F, T, E, A]
object Codec extends CodecDerivation:
  trait AsObject[F[_]: Functor, S, T, E, A] extends Codec[F, S, T, E, A] with Encoder.AsObject[F, S, A]
  object AsObject:
    type Aux[F[_], S, O, T, E, A] = AsObject[F, S, T, E, A] { type Object = O }
  end AsObject
end Codec
