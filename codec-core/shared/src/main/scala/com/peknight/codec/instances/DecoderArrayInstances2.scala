package com.peknight.codec.instances

import cats.{Applicative, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.ArrayType

import scala.collection.immutable.ArraySeq

trait DecoderArrayInstances2:
  given decodeUntaggedArraySeqA[F[_], S, A](using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S],
    show: Show[S]
  ): Decoder[F, Cursor[S], ArraySeq[A]] =
    Decoder.decodeSeq[F, S, A, ArraySeq](ArraySeq.untagged.newBuilder)
end DecoderArrayInstances2
