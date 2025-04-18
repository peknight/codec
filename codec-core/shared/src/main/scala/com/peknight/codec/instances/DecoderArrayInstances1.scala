package com.peknight.codec.instances

import cats.data.OneAnd
import cats.{Applicative, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.ArrayType

import scala.collection.Factory
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

trait DecoderArrayInstances1 extends DecoderArrayInstances2:
  given decodeArrayA[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S],
    show: Show[S],
    factory: Factory[A, Array[A]]
  ): Decoder[F, Cursor[S], Array[A]] =
    Decoder.decodeSeq[F, S, A, Array](factory.newBuilder)

  given decodeOneAndA[F[_], S, A, C[_]](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S],
    show: Show[S],
    factory: Factory[A, C[A]]
  ): Decoder[F, Cursor[S], OneAnd[C, A]] =
    Decoder.decodeNonEmptySeq[F, S, A, C, OneAnd[C, A]](factory.newBuilder)(OneAnd.apply)

  given decodeArraySeqA[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S],
    show: Show[S],
    classTag: ClassTag[A]
  ): Decoder[F, Cursor[S], ArraySeq[A]] =
    Decoder.decodeSeq[F, S, A, ArraySeq](ArraySeq.newBuilder[A])

  given decodeIterableA[F[_], S, A, C[X] <: Iterable[X]](using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S],
    show: Show[S],
    factory: Factory[A, C[A]]
  ): Decoder[F, Cursor[S], C[A]] =
    Decoder.decodeSeq[F, S, A, C](factory.newBuilder)
end DecoderArrayInstances1
