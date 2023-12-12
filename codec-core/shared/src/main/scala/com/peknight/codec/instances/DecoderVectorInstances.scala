package com.peknight.codec.instances

import cats.{Monad, Order}
import cats.data.{Chain, NonEmptyList, NonEmptySet, NonEmptyVector, NonEmptyChain}
import com.peknight.cats.ext.data.ChainBuilder
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.ArrayType

import scala.collection.immutable.SortedSet

trait DecoderVectorInstances:
  given decodeSeq[F[_], S, A](using Monad[F], Decoder[F, Cursor[S], DecodingFailure, A], ArrayType[S])
  : Decoder[F, Cursor[S], DecodingFailure, Seq[A]] =
    Decoder.decodeSeq[F, S, A, Seq](Seq.newBuilder[A])

  given decodeSet[F[_], S, A](using Monad[F], Decoder[F, Cursor[S], DecodingFailure, A], ArrayType[S])
  : Decoder[F, Cursor[S], DecodingFailure, Set[A]] =
    Decoder.decodeSeq[F, S, A, Set](Set.newBuilder[A])

  given decodeList[F[_], S, A](using Monad[F], Decoder[F, Cursor[S], DecodingFailure, A], ArrayType[S])
  : Decoder[F, Cursor[S], DecodingFailure, List[A]] =
    Decoder.decodeSeq[F, S, A, List](List.newBuilder[A])

  given decodeVector[F[_], S, A](using Monad[F], Decoder[F, Cursor[S], DecodingFailure, A], ArrayType[S])
  : Decoder[F, Cursor[S], DecodingFailure, Vector[A]] =
    Decoder.decodeSeq[F, S, A, Vector](Vector.newBuilder[A])

  given decodeChain[F[_], S, A](using Monad[F], Decoder[F, Cursor[S], DecodingFailure, A], ArrayType[S])
  : Decoder[F, Cursor[S], DecodingFailure, Chain[A]] =
    Decoder.decodeSeq[F, S, A, Chain](new ChainBuilder[A])

  given decodeNonEmptyList[F[_], S, A](using Monad[F], Decoder[F, Cursor[S], DecodingFailure, A], ArrayType[S])
  : Decoder[F, Cursor[S], DecodingFailure, NonEmptyList[A]] =
    Decoder.decodeNonEmptySeq[F, S, A, List, NonEmptyList[A]](List.newBuilder[A])(NonEmptyList.apply)

  given decodeNonEmptyVector[F[_], S, A](using Monad[F], Decoder[F, Cursor[S], DecodingFailure, A], ArrayType[S])
  : Decoder[F, Cursor[S], DecodingFailure, NonEmptyVector[A]] =
    Decoder.decodeNonEmptySeq[F, S, A, Vector, NonEmptyVector[A]](Vector.newBuilder[A])(NonEmptyVector.apply)

  given decodeNonEmptySet[F[_], S, A](using monad: Monad[F], decoder: Decoder[F, Cursor[S], DecodingFailure, A],
                                      arrayType: ArrayType[S], order: Order[A])
  : Decoder[F, Cursor[S], DecodingFailure, NonEmptySet[A]] =
    Decoder.decodeNonEmptySeq[F, S, A, SortedSet, NonEmptySet[A]](
      SortedSet.newBuilder[A](Order.catsKernelOrderingForOrder(order))
    )(NonEmptySet.apply)

  given decodeNonEmptyChain[F[_], S, A](using monad: Monad[F], decoder: Decoder[F, Cursor[S], DecodingFailure, A],
                                      arrayType: ArrayType[S], order: Order[A])
  : Decoder[F, Cursor[S], DecodingFailure, NonEmptyChain[A]] =
    Decoder.decodeNonEmptySeq[F, S, A, Chain, NonEmptyChain[A]](new ChainBuilder[A])(NonEmptyChain.fromChainPrepend)
end DecoderVectorInstances
