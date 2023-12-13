package com.peknight.codec.instances

import cats.{Applicative, Monad}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

import scala.collection.{Factory, Map}

trait DecoderObjectInstances1 extends DecoderObjectInstances2:

  given decodeMapLike[F[_], S, K, V, M[X, Y] <: Map[X, Y]](using monad: Monad[F],
                                                           keyDecoder: Decoder[F, String, DecodingFailure, K],
                                                           valueDecoder: Decoder[F, Cursor[S], DecodingFailure, V],
                                                           objectType: ObjectType[S],
                                                           factory: Factory[(K, V), M[K, V]])
  : Decoder[F, Cursor[S], DecodingFailure, M[K, V]] =
    Decoder.decodeMap[F, S, K, V, M](factory.newBuilder)

  given decodeUnitOA[F[_], S](using Applicative[F], ObjectType[S], ArrayType[S])
  : Decoder[F, Cursor[S], DecodingFailure, Unit] =
    Decoder.cursor[F, S, Unit](decodeUnit(_)(s => objectUnit(s).orElse(arrayUnit(s))))

  given decodeUnitON[F[_], S](using Applicative[F], ObjectType[S], NullType[S])
  : Decoder[F, Cursor[S], DecodingFailure, Unit] =
    Decoder.cursor[F, S, Unit](decodeUnit(_)(s => objectUnit(s).orElse(nullUnit(s))))

  given decodeUnitAN[F[_], S](using Applicative[F], ArrayType[S], NullType[S])
  : Decoder[F, Cursor[S], DecodingFailure, Unit] =
    Decoder.cursor[F, S, Unit](decodeUnit(_)(s => arrayUnit(s).orElse(nullUnit(s))))
end DecoderObjectInstances1
