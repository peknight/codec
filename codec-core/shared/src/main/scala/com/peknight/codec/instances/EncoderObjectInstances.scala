package com.peknight.codec.instances

import cats.data.NonEmptyMap
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Applicative, Functor}
import com.peknight.codec.sum.ObjectType
import com.peknight.codec.{Encoder, Object}
import com.peknight.generic.priority.LowPriority

import scala.collection.Map
import scala.collection.immutable.Map as ImmutableMap

trait EncoderObjectInstances:
  given encodeUnit[F[_]: Applicative, S]: Encoder[F, Object[S], Unit] with
    def encode(a: Unit): F[Object[S]] = Object.empty[S].pure[F]
  end encodeUnit

  given encodeNonEmptyMap[F[_], S, K, V](using applicative: Applicative[F], keyEncoder: Encoder[F, String, K],
                                         valueEncoder: Encoder[F, S, V]): Encoder[F, Object[S], NonEmptyMap[K, V]] =
    encodeMap[F, S, K, V].contramap(_.toSortedMap)

  given encodeMap[F[_], S, K, V](using applicative: Applicative[F], keyEncoder: Encoder[F, String, K],
                                 valueEncoder: Encoder[F, S, V]): Encoder[F, Object[S], ImmutableMap[K, V]] =
    encodeMapLike[F, S, K, V, ImmutableMap](using applicative, keyEncoder, valueEncoder, identity)

  given encodeMapLike[F[_], S, K, V, M[X, Y] <: Map[X, Y]](using applicative: Applicative[F],
                                                           keyEncoder: Encoder[F, String, K],
                                                           valueEncoder: Encoder[F, S, V],
                                                           ev: M[K, V] => Iterable[(K, V)])
  : Encoder[F, Object[S], M[K, V]] with
    def encode(a: M[K, V]): F[Object[S]] =
      ev(a).toVector.traverse[F, (String, S)] {
        case (k, v) => (keyEncoder.encode(k), valueEncoder.encode(v)).mapN((_, _))
      }.map(Object.fromIterable)

  given objectEncoder[F[_], S, A](using functor: Functor[F], encoder: Encoder[F, Object[S], A], objectType: ObjectType[S])
  : LowPriority[Encoder[F, S, A]] =
    LowPriority((a: A) => encoder.encode(a).map(obj => objectType.to(objectType.fromObject(obj))))
  end objectEncoder
end EncoderObjectInstances
