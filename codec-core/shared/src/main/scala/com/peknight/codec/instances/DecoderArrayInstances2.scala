package com.peknight.codec.instances

import cats.Monad
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.ArrayType

import scala.collection.immutable.ArraySeq

trait DecoderArrayInstances2:
  given decodeUntaggedArraySeq[F[_], S, A](using
    monad: Monad[F],
    decoder: Decoder[F, Cursor[S], DecodingFailure, A],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], DecodingFailure, ArraySeq[A]] =
    Decoder.decodeSeq[F, S, A, ArraySeq](ArraySeq.untagged.newBuilder)
end DecoderArrayInstances2
