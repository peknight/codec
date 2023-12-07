package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.Encoder
import com.peknight.codec.sum.NullType
import com.peknight.generic.priority.MidPriority

trait EncoderNullInstances:
  given encodeOption[F[_], S, A](using applicative: Applicative[F], encoder: Encoder[F, S, A], nullType: NullType[S])
  : MidPriority[Encoder[F, S, Option[A]]] =
    MidPriority(_.fold(nullType.unit.pure[F])(encoder.encode))
  end encodeOption
  given encodeSome[F[_], S, A](using applicative: Applicative[F], encoder: Encoder[F, S, A])
  : MidPriority[Encoder[F, S, Some[A]]] =
    MidPriority(a => encoder.encode(a.value))
  end encodeSome
  given encodeNone[F[_], S](using Applicative[F], NullType[S])
  : MidPriority[Encoder[F, S, None.type]] =
    MidPriority(_ => NullType[S].unit.pure[F])
  end encodeNone
end EncoderNullInstances
