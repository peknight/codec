package com.peknight.codec.instances

import cats.Applicative
import cats.data.ValidatedNel
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.validated.*
import com.peknight.codec.cursor.{Cursor, CursorDecoder, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, NotNull, NotObject}
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}
import com.peknight.codec.{Decoder, Object}
import com.peknight.generic.priority.MidPriority

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
      else NotNull(t).asLeft.pure
    }

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
