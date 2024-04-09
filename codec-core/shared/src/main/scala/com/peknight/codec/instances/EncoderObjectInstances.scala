package com.peknight.codec.instances

import cats.Applicative
import cats.data.NonEmptyMap
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import com.peknight.codec.sum.ObjectType
import com.peknight.codec.Encoder
import com.peknight.codec.obj.Object

import scala.collection.Map
import scala.collection.immutable.Map as ImmutableMap

trait EncoderObjectInstances:
  given objectEncodeUnit[F[_] : Applicative, S]: Encoder[F, Object[S], Unit] with
    def encode(a: Unit): F[Object[S]] = Object.empty[S].pure[F]
  end objectEncodeUnit

  given encodeUnit[F[_]: Applicative, S: ObjectType]: Encoder[F, S, Unit] =
    Encoder.objectEncoder[F, S, Unit]

  given objectEncodeNonEmptyMap[F[_], S, K, V](
    using applicative: Applicative[F], keyEncoder: Encoder[F, String, K], valueEncoder: Encoder[F, S, V]
  ): Encoder[F, Object[S], NonEmptyMap[K, V]] =
    objectEncodeMap[F, S, K, V].contramap(_.toSortedMap)

  given encodeNonEmptyMap[F[_], S, K, V](using Applicative[F], Encoder[F, String, K], Encoder[F, S, V], ObjectType[S])
  : Encoder[F, S, NonEmptyMap[K, V]] =
    Encoder.objectEncoder[F, S, NonEmptyMap[K, V]]

  given objectEncodeMap[F[_], S, K, V](
    using applicative: Applicative[F], keyEncoder: Encoder[F, String, K], valueEncoder: Encoder[F, S, V]
  ): Encoder[F, Object[S], ImmutableMap[K, V]] =
    objectEncodeMapLike[F, S, K, V, ImmutableMap](using applicative, keyEncoder, valueEncoder, identity)

  given encodeMap[F[_], S, K, V](using Applicative[F], Encoder[F, String, K], Encoder[F, S, V], ObjectType[S])
  : Encoder[F, S, ImmutableMap[K, V]] =
    Encoder.objectEncoder[F, S, ImmutableMap[K, V]]

  given objectEncodeMapLike[F[_], S, K, V, M[X, Y] <: Map[X, Y]](
    using
    applicative: Applicative[F],
    keyEncoder: Encoder[F, String, K],
    valueEncoder: Encoder[F, S, V],
    ev: M[K, V] => Iterable[(K, V)]
  ): Encoder[F, Object[S], M[K, V]] with
    def encode(a: M[K, V]): F[Object[S]] =
      ev(a).toVector.traverse[F, (String, S)] {
        case (k, v) => (keyEncoder.encode(k), valueEncoder.encode(v)).mapN((_, _))
      }.map(Object.fromIterable)
  end objectEncodeMapLike

  given encodeMapLike[F[_], S, K, V, M[X, Y] <: Map[X, Y]](
    using Applicative[F], Encoder[F, String, K], Encoder[F, S, V], M[K, V] => Iterable[(K, V)], ObjectType[S]
  ): Encoder[F, S, M[K, V]] =
    Encoder.objectEncoder[F, S, M[K, V]]

  given encodeObject[F[_], S, O](using applicative: Applicative[F], objectType: ObjectType.Aux[S, O])
  : Encoder[F, S, O] with
    def encode(a: O): F[S] = objectType.to(a).pure[F]
  end encodeObject
end EncoderObjectInstances
