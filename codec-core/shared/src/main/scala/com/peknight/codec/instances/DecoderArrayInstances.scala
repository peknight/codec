package com.peknight.codec.instances

import cats.data.*
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.{Applicative, Order}
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
  given decodeSetA[F[_], S, A](using Applicative[F], Decoder[F, Cursor[S], A], ArrayType[S])
  : Decoder[F, Cursor[S], Set[A]] =
    Decoder.decodeSeq[F, S, A, Set](Set.newBuilder[A])

  given decodeListA[F[_], S, A](using Applicative[F], Decoder[F, Cursor[S], A], ArrayType[S])
  : Decoder[F, Cursor[S], List[A]] =
    Decoder.decodeSeq[F, S, A, List](List.newBuilder[A])

  given decodeVectorA[F[_], S, A](using Applicative[F], Decoder[F, Cursor[S], A], ArrayType[S])
  : Decoder[F, Cursor[S], Vector[A]] =
    Decoder.decodeSeq[F, S, A, Vector](Vector.newBuilder[A])

  given decodeChainA[F[_], S, A](using Applicative[F], Decoder[F, Cursor[S], A], ArrayType[S])
  : Decoder[F, Cursor[S], Chain[A]] =
    Decoder.decodeSeq[F, S, A, Chain](new ChainBuilder[A])

  given decodeNonEmptyListA[F[_], S, A](using Applicative[F], Decoder[F, Cursor[S], A], ArrayType[S])
  : Decoder[F, Cursor[S], NonEmptyList[A]] =
    Decoder.decodeNonEmptySeq[F, S, A, List, NonEmptyList[A]](List.newBuilder[A])(NonEmptyList.apply)

  given decodeNonEmptyVectorA[F[_], S, A](using Applicative[F], Decoder[F, Cursor[S], A], ArrayType[S])
  : Decoder[F, Cursor[S], NonEmptyVector[A]] =
    Decoder.decodeNonEmptySeq[F, S, A, Vector, NonEmptyVector[A]](Vector.newBuilder[A])(NonEmptyVector.apply)

  given decodeNonEmptySetA[F[_], S, A](using applicative: Applicative[F], decoder: Decoder[F, Cursor[S], A],
                                       arrayType: ArrayType[S], order: Order[A])
  : Decoder[F, Cursor[S], NonEmptySet[A]] =
    Decoder.decodeNonEmptySeq[F, S, A, SortedSet, NonEmptySet[A]](
      SortedSet.newBuilder[A](Order.catsKernelOrderingForOrder(order))
    )(NonEmptySet.apply)

  given decodeNonEmptyChainA[F[_], S, A](using applicative: Applicative[F], decoder: Decoder[F, Cursor[S], A],
                                         arrayType: ArrayType[S], order: Order[A])
  : Decoder[F, Cursor[S], NonEmptyChain[A]] =
    Decoder.decodeNonEmptySeq[F, S, A, Chain, NonEmptyChain[A]](new ChainBuilder[A])(NonEmptyChain.fromChainPrepend)

  given decodeSeqA[F[_], S, A](using Applicative[F], Decoder[F, Cursor[S], A], ArrayType[S])
  : Decoder[F, Cursor[S], Seq[A]] =
    Decoder.decodeSeq[F, S, A, Seq](Seq.newBuilder[A])

  given decodeTupleA[F[_], S, T <: Tuple](
    using
    applicative: Applicative[F],
    arrayType: ArrayType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], T]
  ): Decoder[F, Cursor[S], T] =
    case cursor: SuccessCursor[S] => arrayType.asArray(cursor.value) match
      case Some(vector) if vector.size == instances.size =>
        instances.constructWithIndex[[X] =>> F[Validated[DecodingFailure, X]]] {
          [X] => (ft: Decoder[F, Cursor[S], X], index: Int) => ft.decode(cursor.downN(index)).map(_.toValidated)
        }.map(_.toEither)
      case Some(vector) => TupleSizeNotMatch(instances.size, vector.size).asLeft.pure[F]
      case None => NotArray.cursor(cursor).asLeft.pure[F]
    case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]
  end decodeTupleA

end DecoderArrayInstances
