package com.peknight.codec

import cats.Functor
import cats.syntax.functor.*
import cats.data.ValidatedNel
import cats.syntax.either.*
import com.peknight.codec.derivation.CodecDerivation

trait Codec[F[_], S, T, E, A] extends Encoder[F, S, A] with Decoder[F, T, E, A]
object Codec extends CodecDerivation:
  def apply[F[_], S, T, E, A](using codec: Codec[F, S, T, E, A]): Codec[F, S, T, E, A] = codec
  def apply[F[_], S, T, E, A](
    encode0: A => F[S], decode0: T => F[Either[E, A]], decodeAccumulating0: T => F[ValidatedNel[E, A]]
  ): Codec[F, S, T, E, A] =
    new Codec[F, S, T, E, A]:
      def encode(a: A): F[S] = encode0(a)
      def decode(t: T): F[Either[E, A]] = decode0(t)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = decodeAccumulating0(t)
  end apply

  def apply[F[_]: Functor, S, T, E, A](encode: A => F[S], decode: T => F[Either[E, A]]): Codec[F, S, T, E, A] =
    apply(encode, decode, decode.andThen(_.map(_.toValidatedNel)))

  def apply[F[_], S, T, E, A](encoder: Encoder[F, S, A], decoder: Decoder[F, T, E, A]): Codec[F, S, T, E, A] =
    apply(encoder.encode, decoder.decode, decoder.decodeAccumulating)
end Codec
