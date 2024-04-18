package com.peknight.codec.instances

import cats.Applicative
import cats.data.NonEmptyMap
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import com.peknight.codec.Encoder
import com.peknight.codec.obj.Object
import com.peknight.codec.sum.ObjectType

import scala.collection.Map
import scala.collection.immutable.Map as ImmutableMap

trait EncoderObjectInstances:
  given encodeObjectO[F[_], S, O](using applicative: Applicative[F], objectType: ObjectType.Aux[S, O])
  : Encoder[F, S, O] =
    Encoder.map(objectType.to)

  given objectEncodeUnit[F[_] : Applicative, S]: Encoder[F, Object[S], Unit] =
    Encoder.const[F, Object[S], Unit](Object.empty[S])

  given encodeUnitO[F[_]: Applicative, S: ObjectType]: Encoder[F, S, Unit] =
    Encoder.encodeO[F, S, Unit]

  given objectEncodeNonEmptyMap[F[_], S, K, V](
    using applicative: Applicative[F], keyEncoder: Encoder[F, String, K], valueEncoder: Encoder[F, S, V]
  ): Encoder[F, Object[S], NonEmptyMap[K, V]] =
    objectEncodeMap[F, S, K, V].contramap(_.toSortedMap)

  given encodeNonEmptyMapO[F[_], S, K, V](using Applicative[F], Encoder[F, String, K], Encoder[F, S, V], ObjectType[S])
  : Encoder[F, S, NonEmptyMap[K, V]] =
    Encoder.encodeO[F, S, NonEmptyMap[K, V]]

  given objectEncodeMap[F[_], S, K, V](
    using applicative: Applicative[F], keyEncoder: Encoder[F, String, K], valueEncoder: Encoder[F, S, V]
  ): Encoder[F, Object[S], ImmutableMap[K, V]] =
    objectEncodeMapLike[F, S, K, V, ImmutableMap](using applicative, keyEncoder, valueEncoder, identity)

  given encodeMapO[F[_], S, K, V](using Applicative[F], Encoder[F, String, K], Encoder[F, S, V], ObjectType[S])
  : Encoder[F, S, ImmutableMap[K, V]] =
    Encoder.encodeO[F, S, ImmutableMap[K, V]]

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

  given encodeMapLikeO[F[_], S, K, V, M[X, Y] <: Map[X, Y]](
    using Applicative[F], Encoder[F, String, K], Encoder[F, S, V], M[K, V] => Iterable[(K, V)], ObjectType[S]
  ): Encoder[F, S, M[K, V]] =
    Encoder.encodeO[F, S, M[K, V]]
end EncoderObjectInstances
