package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.codec.cursor.{Cursor, Decoder, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, NotNull}
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderObjectInstances1 extends DecoderObjectInstances2:
  given decodeUnitOA[F[_], S](using Applicative[F], ObjectType[S], ArrayType[S]): Decoder[F, S, Unit] =
    Decoder.instance[F, S, Unit](decodeUnit(_)(s => objectUnit(s).orElse(arrayUnit(s))))

  given decodeUnitON[F[_], S](using Applicative[F], ObjectType[S], NullType[S]): Decoder[F, S, Unit] =
    Decoder.instance[F, S, Unit](decodeUnit(_)(s => objectUnit(s).orElse(nullUnit(s))))

  given decodeUnitAN[F[_], S](using Applicative[F], ArrayType[S], NullType[S]): Decoder[F, S, Unit] =
    Decoder.instance[F, S, Unit](decodeUnit(_)(s => arrayUnit(s).orElse(nullUnit(s))))
end DecoderObjectInstances1
