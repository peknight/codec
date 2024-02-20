package com.peknight.codec.instances

import cats.Applicative
import cats.data.*
import com.peknight.codec.Encoder
import com.peknight.codec.sum.ArrayType
import com.peknight.generic.Generic
import com.peknight.generic.priority.HighPriority

trait EncoderArrayInstances extends EncoderVectorInstances:
  given encodeSeq[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, Seq[A]] =
    Encoder.arrayEncoder[F, S, Seq[A]](encodeVectorSeq[F, S, A])

  given encodeSet[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, Set[A]] =
    Encoder.arrayEncoder[F, S, Set[A]](encodeVectorSet[F, S, A])

  given encodeList[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, List[A]] =
    Encoder.arrayEncoder[F, S, List[A]](encodeVectorList[F, S, A])

  given encodeVector[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, Vector[A]] =
    Encoder.arrayEncoder[F, S, Vector[A]](encodeVectorVector[F, S, A])

  given encodeChain[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, Chain[A]] =
    Encoder.arrayEncoder[F, S, Chain[A]](encodeVectorChain[F, S, A])

  given encodeNonEmptyList[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, NonEmptyList[A]] =
    Encoder.arrayEncoder[F, S, NonEmptyList[A]](encodeVectorNonEmptyList[F, S, A])

  given encodeNonEmptyVector[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, NonEmptyVector[A]] =
    Encoder.arrayEncoder[F, S, NonEmptyVector[A]](encodeVectorNonEmptyVector[F, S, A])

  given encodeNonEmptySet[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, NonEmptySet[A]] =
    Encoder.arrayEncoder[F, S, NonEmptySet[A]](encodeVectorNonEmptySet[F, S, A])

  given encodeNonEmptyChain[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, NonEmptyChain[A]] =
    Encoder.arrayEncoder[F, S, NonEmptyChain[A]](encodeVectorNonEmptyChain[F, S, A])

  given encodeOneAnd[F[_], S, A, G[_]](using applicative: Applicative[F], encoder: Encoder[F, S, A],
                                       ev: G[A] => Iterable[A], arrayType: ArrayType[S]): Encoder[F, S, OneAnd[G, A]] =
    Encoder.arrayEncoder[F, S, OneAnd[G, A]](encodeVectorOneAnd[F, S, A, G])

  given encodeIterable[F[_], S, A, G[_]](using applicative: Applicative[F], encoder: Encoder[F, S, A],
                                         ev: G[A] => Iterable[A],
                                         arrayType: ArrayType[S]): HighPriority[Encoder[F, S, G[A]]] =
    HighPriority(Encoder.arrayEncoder[F, S, G[A]](encodeVectorIterable[F, S, A, G].instance))

  given encodeTuple[F[_], S, A <: Tuple](using applicative: Applicative[F], arrayType: ArrayType[S],
                                         instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A])
  : Encoder[F, S, A] =
    Encoder.arrayEncoder[F, S, A](encodeVectorTuple[F, S, A])
end EncoderArrayInstances
