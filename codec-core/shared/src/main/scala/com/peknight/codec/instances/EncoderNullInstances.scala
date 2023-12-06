package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.Encoder
import com.peknight.codec.sum.NullType

trait EncoderNullInstances:
  given encodeOption[F[_], S, A](using applicative: Applicative[F], encoder: Encoder[F, S, A], nullType: NullType[S])
  : Encoder[F, S, Option[A]] with
    def encode(a: Option[A]): F[S] = a.fold(nullType.unit.pure[F])(encoder.encode)
  end encodeOption
  given encodeSome[F[_], S, A](using applicative: Applicative[F], encoder: Encoder[F, S, A]): Encoder[F, S, Some[A]] with
    def encode(a: Some[A]): F[S] = encoder.encode(a.value)
  end encodeSome
  given encodeNone[F[_], S](using applicative: Applicative[F], nullType: NullType[S]): Encoder[F, S, None.type] with
    def encode(a: None.type): F[S] = nullType.unit.pure[F]
  end encodeNone
end EncoderNullInstances
