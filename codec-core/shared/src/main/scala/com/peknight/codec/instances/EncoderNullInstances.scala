package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.Encoder
import com.peknight.codec.sum.NullType
import com.peknight.generic.priority.LowPriority

trait EncoderNullInstances:
  given encodeOption[F[_], S, A](using applicative: Applicative[F], encoder: Encoder[F, S, A], nullType: NullType[S])
  : LowPriority[Encoder[F, S, Option[A]]] =
    LowPriority((a: Option[A]) => a.fold(nullType.unit.pure[F])(encoder.encode))
  end encodeOption
  given encodeSome[F[_], S, A](using applicative: Applicative[F], encoder: Encoder[F, S, A])
  : LowPriority[Encoder[F, S, Some[A]]] =
    LowPriority((a: Some[A]) => encoder.encode(a.value))
  end encodeSome
  given encodeNone[F[_], S](using applicative: Applicative[F], nullType: NullType[S])
  : LowPriority[Encoder[F, S, None.type]] =
    LowPriority((a: None.type) => nullType.unit.pure[F])
  end encodeNone
end EncoderNullInstances
