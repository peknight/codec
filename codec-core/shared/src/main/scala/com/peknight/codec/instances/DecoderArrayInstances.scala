package com.peknight.codec.instances

import cats.data.*
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.validated.*
import cats.{Applicative, Monad, Order}
import com.peknight.cats.ext.data.ChainBuilder
import com.peknight.cats.ext.instances.applicative.given
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, NotArray, TupleSizeNotMatch}
import com.peknight.codec.sum.ArrayType
import com.peknight.generic.Generic

import scala.collection.immutable.SortedSet

trait DecoderArrayInstances extends DecoderArrayInstances1:
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

  given decodeTuple[F[_], S, T <: Tuple](using
    monad: Applicative[F],
    arrayType: ArrayType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], T]
  ): Decoder[F, Cursor[S], DecodingFailure, T] =
    new Decoder[F, Cursor[S], DecodingFailure, T]:
      def decode(t: Cursor[S]): F[Either[DecodingFailure, T]] =
        t match
          case cursor: SuccessCursor[S] => arrayType.asArray(cursor.value) match
            case Some(vector) if vector.size == instances.size =>
              instances.constructWithIndex[[X] =>> F[Either[DecodingFailure, X]]] {
                [X] => (ft: Decoder[F, Cursor[S], DecodingFailure, X], index: Int) => ft.decode(cursor.downN(index))
              }
            case Some(vector) => TupleSizeNotMatch(instances.size, vector.size).asLeft.pure[F]
            case None => NotArray.cursor(cursor).asLeft.pure[F]
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]
      def decodeAccumulating(t: Cursor[S]): F[ValidatedNel[DecodingFailure, T]] =
        t match
          case cursor: SuccessCursor[S] => arrayType.asArray(cursor.value) match
            case Some(vector) if vector.size == instances.size =>
              instances.constructWithIndex[[X] =>> F[ValidatedNel[DecodingFailure, X]]] {
                [X] => (ft: Decoder[F, Cursor[S], DecodingFailure, X], index: Int) =>
                  ft.decodeAccumulating(cursor.downN(index))
              }
            case Some(vector) => TupleSizeNotMatch(instances.size, vector.size).invalidNel.pure[F]
            case None => NotArray.cursor(cursor).invalidNel.pure[F]
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.invalidNel.pure[F]
  end decodeTuple

end DecoderArrayInstances
