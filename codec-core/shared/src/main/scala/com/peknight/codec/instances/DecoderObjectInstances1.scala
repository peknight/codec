package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

import scala.collection.{Factory, Map}

trait DecoderObjectInstances1 extends DecoderObjectInstances2:
  given decodeMapLikeO[F[_], S, K, V, M[X, Y] <: Map[X, Y]](
    using
    applicative: Applicative[F],
    keyDecoder: Decoder[F, String, K],
    valueDecoder: Decoder[F, Cursor[S], V],
    objectType: ObjectType[S],
    factory: Factory[(K, V), M[K, V]]
  ): Decoder[F, Cursor[S], M[K, V]] =
    Decoder.decodeMap[F, S, K, V, M](factory.newBuilder)

  given decodeUnitAO[F[_], S](using Applicative[F], ObjectType[S], ArrayType[S])
  : Decoder[F, Cursor[S], Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(s => Decoder.objectUnit(s).orElse(Decoder.arrayUnit(s))))

  given decodeUnitOU[F[_], S](using Applicative[F], ObjectType[S], NullType[S])
  : Decoder[F, Cursor[S], Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(s => Decoder.objectUnit(s).orElse(Decoder.nullUnit(s))))

  given decodeUnitAU[F[_], S](using Applicative[F], ArrayType[S], NullType[S])
  : Decoder[F, Cursor[S], Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(s => Decoder.arrayUnit(s).orElse(Decoder.nullUnit(s))))
end DecoderObjectInstances1
