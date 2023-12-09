package com.peknight.codec.instances

import cats.Applicative
import cats.data.ValidatedNel
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import com.peknight.codec.cursor.CursorDecoder
import com.peknight.codec.error.{DecodingFailure, NotObject, NotUnit}
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}
import com.peknight.codec.{Decoder, Object}
import com.peknight.generic.priority.MidPriority

import scala.collection.Map

trait DecoderObjectInstances extends DecoderObjectInstances1:

  given decodeObject[F[_], S, O](using applicative: Applicative[F], objectType: ObjectType.Aux[S, O])
  : CursorDecoder[F, S, O] =
    CursorDecoder.instance[F, S, O] { t =>
      objectType.asObject(t.value) match
        case Some(o) => o.asRight.pure
        case None => NotObject(t).asLeft.pure
    }

  given decodeUnit[F[_], S](using Applicative[F], ObjectType[S], ArrayType[S], NullType[S]):
    CursorDecoder[F, S, Unit] =
    CursorDecoder.instance[F, S, Unit](decodeUnit(_)(s => objectUnit(s).orElse(arrayUnit(s)).orElse(nullUnit(s))))

  given decodeObjectUnit[F[_]: Applicative, S]: Decoder[F, Object[S], DecodingFailure[Object[S]], Unit] =
    Decoder.instance[F, Object[S], DecodingFailure[Object[S]], Unit] { t =>
      if t.isEmpty then ().asRight.pure
      else NotUnit(t).asLeft.pure
    }

  given decodeMap[F[_], S, K, V, M[X, Y] <: Map[X, Y]](using keyDecoder: Decoder[F, String, DecodingFailure[String], K],
                                                       valueDecoder: Decoder[F, S, DecodingFailure[S], V])
  : Decoder[F, Object[S], DecodingFailure[Object[S]], M[K, V]] =
    new Decoder[F, Object[S], DecodingFailure[Object[S]], M[K, V]]:
      override def decode(t: Object[S]): F[Either[DecodingFailure[Object[S]], M[K, V]]] = ???
      override def decodeAccumulating(t: Object[S]): F[ValidatedNel[DecodingFailure[Object[S]], M[K, V]]] = ???
  end decodeMap

  given objectDecoder[F[_], S, A](using applicative: Applicative[F],
                                  decoder: Decoder[F, Object[S], DecodingFailure[Object[S]], A],
                                  objectType: ObjectType.Aux[S, Object[S]])
  : MidPriority[CursorDecoder[F, S, A]] =
    MidPriority { CursorDecoder.instance[F, S, A] { t =>
      objectType.asObject(t.value) match
        case Some(o) => decoder.decode(o).map(_.left.map(_.as(t)))
        case None => NotObject(t).asLeft.pure
    }}

end DecoderObjectInstances
