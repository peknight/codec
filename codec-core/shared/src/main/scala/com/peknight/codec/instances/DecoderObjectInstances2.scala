package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderObjectInstances2 extends DecoderObjectInstances3:
  given decodeUnitO[F[_], S](using Applicative[F], ObjectType[S]): Decoder[F, Cursor[S], DecodingFailure, Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(Decoder.objectUnit))

  given decodeUnitA[F[_], S](using Applicative[F], ArrayType[S]): Decoder[F, Cursor[S], DecodingFailure, Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(Decoder.arrayUnit))

  given decodeUnitN[F[_], S](using Applicative[F], NullType[S]): Decoder[F, Cursor[S], DecodingFailure, Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(Decoder.nullUnit))
end DecoderObjectInstances2
