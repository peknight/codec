package com.peknight.codec.instances

import cats.Applicative
import cats.data.NonEmptyMap
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import com.peknight.codec.sum.ObjectType
import com.peknight.codec.{Encoder, Object}

import scala.collection.Map
import scala.collection.immutable.Map as ImmutableMap

trait EncoderObjectInstances1:
  given objectEncodeUnit[F[_]: Applicative, S]: Encoder[F, Object[S], Unit] with
    def encode(a: Unit): F[Object[S]] = Object.empty[S].pure[F]
  end objectEncodeUnit

  given objectEncodeNonEmptyMap[F[_], S, K, V](
    using applicative: Applicative[F], keyEncoder: Encoder[F, String, K], valueEncoder: Encoder[F, S, V]
  ): Encoder[F, Object[S], NonEmptyMap[K, V]] =
    objectEncodeMap[F, S, K, V].contramap(_.toSortedMap)

  given objectEncodeMap[F[_], S, K, V](
    using applicative: Applicative[F], keyEncoder: Encoder[F, String, K], valueEncoder: Encoder[F, S, V]
  ): Encoder[F, Object[S], ImmutableMap[K, V]] =
    objectEncodeMapLike[F, S, K, V, ImmutableMap](using applicative, keyEncoder, valueEncoder, identity)

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

  given encodeObject[F[_], S, O](using applicative: Applicative[F], objectType: ObjectType.Aux[S, O])
  : Encoder[F, S, O] with
    def encode(a: O): F[S] = objectType.to(a).pure[F]
  end encodeObject
end EncoderObjectInstances1
