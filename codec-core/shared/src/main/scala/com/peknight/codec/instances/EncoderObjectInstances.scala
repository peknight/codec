package com.peknight.codec.instances

import cats.Applicative
import cats.data.NonEmptyMap
import com.peknight.codec.Encoder
import com.peknight.codec.sum.ObjectType

import scala.collection.Map
import scala.collection.immutable.Map as ImmutableMap

trait EncoderObjectInstances extends EncoderObjectInstances1:
  given encodeUnit[F[_]: Applicative, S: ObjectType]: Encoder[F, S, Unit] =
    Encoder.objectEncoder[F, S, Unit](objectEncodeUnit[F, S])

  given encodeNonEmptyMap[F[_], S, K, V](using Applicative[F], Encoder[F, String, K], Encoder[F, S, V], ObjectType[S])
  : Encoder[F, S, NonEmptyMap[K, V]] =
    Encoder.objectEncoder[F, S, NonEmptyMap[K, V]](objectEncodeNonEmptyMap[F, S, K, V])

  given encodeMap[F[_], S, K, V](using Applicative[F], Encoder[F, String, K], Encoder[F, S, V], ObjectType[S])
  : Encoder[F, S, ImmutableMap[K, V]] =
    Encoder.objectEncoder[F, S, ImmutableMap[K, V]](objectEncodeMap[F, S, K, V])

  given encodeMapLike[F[_], S, K, V, M[X, Y] <: Map[X, Y]](
    using Applicative[F], Encoder[F, String, K], Encoder[F, S, V], M[K, V] => Iterable[(K, V)], ObjectType[S]
  ): Encoder[F, S, M[K, V]] =
    Encoder.objectEncoder[F, S, M[K, V]](objectEncodeMapLike[F, S, K, V, M])
end EncoderObjectInstances
