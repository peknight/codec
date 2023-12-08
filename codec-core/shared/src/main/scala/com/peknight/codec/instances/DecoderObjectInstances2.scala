package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import com.peknight.codec.cursor.{Cursor, Decoder, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, NotNull}
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderObjectInstances2:
  given decodeUnitO[F[_], S](using Applicative[F], ObjectType[S]): Decoder[F, S, Unit] =
    Decoder.instance[F, S, Unit](decodeUnit(_)(objectUnit))

  given decodeUnitA[F[_], S](using Applicative[F], ArrayType[S]): Decoder[F, S, Unit] =
    Decoder.instance[F, S, Unit](decodeUnit(_)(arrayUnit))

  given decodeUnitN[F[_], S](using Applicative[F], NullType[S]): Decoder[F, S, Unit] =
    Decoder.instance[F, S, Unit](decodeUnit(_)(nullUnit))

  protected[this] def objectUnit[S](s: S)(using objectType: ObjectType[S]): Option[Unit] =
    objectType.asObject(s).filter(objectType.isEmpty).as(())

  protected[this] def arrayUnit[S](s: S)(using arrayType: ArrayType[S]): Option[Unit] =
    arrayType.asArray(s).filter(_.isEmpty).as(())

  protected[this] def nullUnit[S](s: S)(using nullType: NullType[S]): Option[Unit] = nullType.asNull(s)

  protected[this] def decodeUnit[F[_]: Applicative, S](t: SuccessCursor[S])(f: S => Option[Unit])
  : F[Either[DecodingFailure[Cursor[S]], Unit]] =
    f(t.value) match
      case Some(_) => ().asRight.pure
      case None => NotNull(t).asLeft.pure
end DecoderObjectInstances2
