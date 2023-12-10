package com.peknight.codec.instances

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import com.peknight.cats.ext.instances.applicative.given
import com.peknight.codec.cursor.CursorDecoder
import com.peknight.codec.error.*
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}
import com.peknight.codec.{Decoder, Object}
import com.peknight.generic.priority.MidPriority

import scala.collection.{Map, mutable}

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

  private[this] def cursorDecodeMap[F[_], S, O, K, V, M[X, Y] <: Map[X, Y]]
  (builder: => mutable.Builder[(K, V), M[K, V]])(using
    applicative: Applicative[F],
    keyDecoder: Decoder[F, String, DecodingFailure[String], K],
    valueDecoder: CursorDecoder[F, S, V],
    objectType: ObjectType[S]
  ): CursorDecoder[F, S, M[K, V]] =
    CursorDecoder.instance[F, S, M[K, V]] { t =>
      objectType.asObject(t.value) match
        case Some(o) =>
          objectType.toList(o).traverse[[T] =>> F[Either[DecodingMapFailure[Unit, S], T]], (K, V)] {
            case (key, value) =>
              (keyDecoder.decode(key), valueDecoder.decode(t.downField(key))).mapN {
                case (Right(key), Right(value)) => ((key, value)).asRight
                case (Right(_), Left(e)) => DecodingMapFailure((), NonEmptyList.one(e.as((key, value)))).asLeft
                case (Left(e), Right(_)) => DecodingMapFailure((), NonEmptyList.one(e.as((key, value)))).asLeft
                case (Left(keyE), Left(valueE)) =>
                  DecodingMapFailure((), NonEmptyList.one(DecodingEntryFailure((key, value), keyE, valueE))).asLeft
              }
          }.map(either => either.map(_.foldLeft(builder)(_ += _).result()).left.map(_.as(t)))
        case _ => NotObject(t).asLeft.pure
    }
  end cursorDecodeMap
  private[this] def decodeMap[F[_], S, K, V, M[X, Y] <: Map[X, Y]](builder: => mutable.Builder[(K, V), M[K, V]])(using
    applicative: Applicative[F],
    keyDecoder: Decoder[F, String, DecodingFailure[String], K],
    valueDecoder: Decoder[F, S, DecodingFailure[S], V]
  ): Decoder[F, Object[S], DecodingFailure[Object[S]], M[K, V]] =
    Decoder.instance[F, Object[S], DecodingFailure[Object[S]], M[K, V]] { t =>
      t.toList.traverse[[T] =>> F[Either[DecodingMapFailure[Unit, S], T]], (K, V)] {
        case (key, value) =>
          (keyDecoder.decode(key), valueDecoder.decode(value)).mapN {
            case (Right(key), Right(value)) => ((key, value)).asRight
            case (Right(_), Left(e)) => DecodingMapFailure((), NonEmptyList.one(e.as((key, value)))).asLeft
            case (Left(e), Right(_)) => DecodingMapFailure((), NonEmptyList.one(e.as((key, value)))).asLeft
            case (Left(keyE), Left(valueE)) =>
              DecodingMapFailure((), NonEmptyList.one(DecodingEntryFailure((key, value), keyE, valueE))).asLeft
          }
      }.map(either => either.map(_.foldLeft(builder)(_ += _).result()).left.map(_.as(t)))
    }
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
