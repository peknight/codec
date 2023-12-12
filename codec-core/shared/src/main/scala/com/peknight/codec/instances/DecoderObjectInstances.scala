package com.peknight.codec.instances

import cats.data.NonEmptyMap
import cats.{Applicative, Monad, Order}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.*
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}
import com.peknight.codec.{Decoder, Object}
import com.peknight.generic.priority.MidPriority

import scala.collection.immutable.{SortedMap, Map as ImmutableMap}

trait DecoderObjectInstances extends DecoderObjectInstances1:

  given decodeObject[F[_], S, O](using applicative: Applicative[F], objectType: ObjectType.Aux[S, O])
  : Decoder[F, Cursor[S], DecodingFailure, O] =
    Decoder.cursor[F, S, O] { t =>
      objectType.asObject(t.value) match
        case Some(o) => o.asRight.pure
        case None => NotObject.cursor(t).asLeft.pure
    }

  given decodeUnit[F[_], S](using Applicative[F], ObjectType[S], ArrayType[S], NullType[S]):
    Decoder[F, Cursor[S], DecodingFailure, Unit] =
    Decoder.cursor[F, S, Unit](decodeUnit(_)(s => objectUnit(s).orElse(arrayUnit(s)).orElse(nullUnit(s))))

  given decodeMap[F[_], S, K, V](using Monad[F], Decoder[F, String, DecodingFailure, K],
                                 Decoder[F, Cursor[S], DecodingFailure, V], ObjectType[S])
  : Decoder[F, Cursor[S], DecodingFailure, ImmutableMap[K, V]] =
    Decoder.decodeMap[F, S, K, V, ImmutableMap](ImmutableMap.newBuilder[K, V])

  given decodeNonEmptyMap[F[_], S, K, V](using monad: Monad[F], keyDecoder: Decoder[F, String, DecodingFailure, K],
                                         valueDecoder: Decoder[F, Cursor[S], DecodingFailure, V],
                                         objectType: ObjectType[S], order: Order[K])
  : Decoder[F, Cursor[S], DecodingFailure, NonEmptyMap[K, V]] =
    Decoder.decodeMap[F, S, K, V, SortedMap](SortedMap.newBuilder[K, V](Order.catsKernelOrderingForOrder(order)))
      .emap(map => cursor => NonEmptyMap.fromMap(map).toRight(EmptyMap.cursor(cursor)))

  given decodeObjectUnit[F[_]: Applicative, S]: Decoder[F, Object[S], DecodingFailure, Unit] =
    Decoder.instance[F, Object[S], DecodingFailure, Unit] { t =>
      if t.isEmpty then ().asRight.pure
      else NotUnit.value(t).asLeft.pure
    }

  given objectDecoder[F[_], S, A](using applicative: Applicative[F],
                                  decoder: Decoder[F, Object[S], DecodingFailure, A],
                                  objectType: ObjectType.Aux[S, Object[S]])
  : MidPriority[Decoder[F, Cursor[S], DecodingFailure, A]] =
    MidPriority { Decoder.cursor[F, S, A] { t =>
      objectType.asObject(t.value) match
        case Some(o) => decoder.decode(o).map(_.left.map(_.cursor(t)))
        case None => NotObject.cursor(t).asLeft.pure
    }}

end DecoderObjectInstances
