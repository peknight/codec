package com.peknight.codec.instances

import cats.{Applicative, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderObjectInstances2:
  given decodeUnitO[F[_]: Applicative, S: {ObjectType, Show}]: Decoder[F, Cursor[S], Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(Decoder.objectUnit))

  given decodeUnitA[F[_]: Applicative, S: {ArrayType, Show}]: Decoder[F, Cursor[S], Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(Decoder.arrayUnit))

  given decodeUnitU[F[_]: Applicative, S: {NullType, Show}]: Decoder[F, Cursor[S], Unit] =
    Decoder.cursor[F, S, Unit](Decoder.decodeUnit(_)(Decoder.nullUnit))
end DecoderObjectInstances2
