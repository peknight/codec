package com.peknight.codec.instances

import cats.data.NonEmptyMap
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.{Applicative, Order}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.*
import com.peknight.codec.obj.Object
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

import scala.collection.immutable.{SortedMap, Map as ImmutableMap}

trait DecoderObjectInstances extends DecoderObjectInstances1:
  given decodeObjectO[F[_], S, O](using applicative: Applicative[F], objectType: ObjectType.Aux[S, O])
  : Decoder[F, Cursor[S], O] =
    Decoder.cursorValueApplicative[F, S, O] { t =>
      objectType.asObject(t) match
        case Some(o) => o.asRight
        case None => NotObject.asLeft
    }

  given objectDecodeUnit[F[_]: Applicative, S]: Decoder[F, Object[S], Unit] =
    Decoder.applicative[F, Object[S], Unit] { t =>
      if t.isEmpty then ().asRight
      else NotUnit.value(t).asLeft
    }

  given decodeUnitAOU[F[_], S](using Applicative[F], ObjectType[S], ArrayType[S], NullType[S])
  : Decoder[F, Cursor[S], Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(
      s => Decoder.objectUnit(s).orElse(Decoder.arrayUnit(s)).orElse(Decoder.nullUnit(s)))
    )

  given decodeMapO[F[_], S, K, V](
    using Applicative[F], Decoder[F, String, K], Decoder[F, Cursor[S], V], ObjectType[S]
  ): Decoder[F, Cursor[S], ImmutableMap[K, V]] =
    Decoder.decodeMap[F, S, K, V, ImmutableMap](ImmutableMap.newBuilder[K, V])

  given decodeNonEmptyMapO[F[_], S, K, V](
    using
    applicative: Applicative[F],
    keyDecoder: Decoder[F, String, K],
    valueDecoder: Decoder[F, Cursor[S], V],
    objectType: ObjectType[S],
    order: Order[K]
  ): Decoder[F, Cursor[S], NonEmptyMap[K, V]] =
    Decoder.decodeMap[F, S, K, V, SortedMap](SortedMap.newBuilder[K, V](Order.catsKernelOrderingForOrder(order)))
      .emap(map => cursor => NonEmptyMap.fromMap(map).toRight(EmptyMap.cursor(cursor)))
end DecoderObjectInstances
