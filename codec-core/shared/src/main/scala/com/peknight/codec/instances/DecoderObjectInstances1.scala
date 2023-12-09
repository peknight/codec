package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.cursor.Decoder
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderObjectInstances1 extends DecoderObjectInstances2:
  given decodeUnitOA[F[_], S](using Applicative[F], ObjectType[S], ArrayType[S]): Decoder[F, S, Unit] =
    Decoder.instance[F, S, Unit](decodeUnit(_)(s => objectUnit(s).orElse(arrayUnit(s))))

  given decodeUnitON[F[_], S](using Applicative[F], ObjectType[S], NullType[S]): Decoder[F, S, Unit] =
    Decoder.instance[F, S, Unit](decodeUnit(_)(s => objectUnit(s).orElse(nullUnit(s))))

  given decodeUnitAN[F[_], S](using Applicative[F], ArrayType[S], NullType[S]): Decoder[F, S, Unit] =
    Decoder.instance[F, S, Unit](decodeUnit(_)(s => arrayUnit(s).orElse(nullUnit(s))))
end DecoderObjectInstances1
