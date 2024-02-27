package com.peknight.codec.instances

import cats.Monad
import cats.data.OneAnd
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.ArrayType

import scala.collection.Factory
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

trait DecoderArrayInstances1 extends DecoderArrayInstances2:
  given decodeArray[F[_], S, A](
    using
    monad: Monad[F],
    decoder: Decoder[F, Cursor[S], DecodingFailure, A],
    arrayType: ArrayType[S],
    factory: Factory[A, Array[A]]
  ): Decoder[F, Cursor[S], DecodingFailure, Array[A]] =
    Decoder.decodeSeq[F, S, A, Array](factory.newBuilder)

  given decodeOneAnd[F[_], S, A, C[_]](
    using
    monad: Monad[F],
    decoder: Decoder[F, Cursor[S], DecodingFailure, A],
    arrayType: ArrayType[S],
    factory: Factory[A, C[A]]
  ): Decoder[F, Cursor[S], DecodingFailure, OneAnd[C, A]] =
    Decoder.decodeNonEmptySeq[F, S, A, C, OneAnd[C, A]](factory.newBuilder)(OneAnd.apply)

  given decodeArraySeq[F[_], S, A](
    using
    monad: Monad[F],
    decoder: Decoder[F, Cursor[S], DecodingFailure, A],
    arrayType: ArrayType[S],
    classTag: ClassTag[A]
  ): Decoder[F, Cursor[S], DecodingFailure, ArraySeq[A]] =
    Decoder.decodeSeq[F, S, A, ArraySeq](ArraySeq.newBuilder[A])

  given decodeIterable[F[_], S, A, C[X] <: Iterable[X]](using
    monad: Monad[F],
    decoder: Decoder[F, Cursor[S], DecodingFailure, A],
    arrayType: ArrayType[S],
    factory: Factory[A, C[A]]
  ): Decoder[F, Cursor[S], DecodingFailure, C[A]] =
    Decoder.decodeSeq[F, S, A, C](factory.newBuilder)
end DecoderArrayInstances1
