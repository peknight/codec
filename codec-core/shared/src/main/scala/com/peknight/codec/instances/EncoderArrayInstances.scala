package com.peknight.codec.instances

import cats.Applicative
import cats.data.*
import cats.syntax.traverse.*
import com.peknight.codec.Encoder
import com.peknight.codec.sum.ArrayType
import com.peknight.generic.Generic

trait EncoderArrayInstances extends EncoderArrayInstances1:
  given vectorEncodeSeq[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Seq[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeSeq[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, Seq[A]] =
    Encoder.arrayEncoder[F, S, Seq[A]]

  given vectorEncodeSet[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Set[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeSet[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, Set[A]] =
    Encoder.arrayEncoder[F, S, Set[A]]

  given vectorEncodeList[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], List[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeList[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, List[A]] =
    Encoder.arrayEncoder[F, S, List[A]]

  given vectorEncodeVector[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Vector[A]] =
    Encoder.vectorEncoder[F, S, A]

  given encodeVector[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, Vector[A]] =
    Encoder.arrayEncoder[F, S, Vector[A]]

  given vectorEncodeChain[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Chain[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeChain[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S]): Encoder[F, S, Chain[A]] =
    Encoder.arrayEncoder[F, S, Chain[A]]

  given vectorEncodeNonEmptyList[F[_], S, A](using Applicative[F], Encoder[F, S, A])
  : Encoder[F, Vector[S], NonEmptyList[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toList.toVector)

  given encodeNonEmptyList[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S])
  : Encoder[F, S, NonEmptyList[A]] =
    Encoder.arrayEncoder[F, S, NonEmptyList[A]]

  given vectorEncodeNonEmptyVector[F[_], S, A](using Applicative[F], Encoder[F, S, A])
  : Encoder[F, Vector[S], NonEmptyVector[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeNonEmptyVector[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S])
  : Encoder[F, S, NonEmptyVector[A]] =
    Encoder.arrayEncoder[F, S, NonEmptyVector[A]]

  given vectorEncodeNonEmptySet[F[_], S, A](using Applicative[F], Encoder[F, S, A])
  : Encoder[F, Vector[S], NonEmptySet[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toSortedSet.toVector)

  given encodeNonEmptySet[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S])
  : Encoder[F, S, NonEmptySet[A]] =
    Encoder.arrayEncoder[F, S, NonEmptySet[A]]

  given vectorEncodeNonEmptyChain[F[_], S, A](using Applicative[F], Encoder[F, S, A])
  : Encoder[F, Vector[S], NonEmptyChain[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toChain.toVector)

  given encodeNonEmptyChain[F[_], S, A](using Applicative[F], Encoder[F, S, A], ArrayType[S])
  : Encoder[F, S, NonEmptyChain[A]] =
    Encoder.arrayEncoder[F, S, NonEmptyChain[A]]

  given vectorEncodeOneAnd[F[_], S, A, G[_]](
    using
    applicative: Applicative[F],
    encoder: Encoder[F, S, A],
    ev: G[A] => Iterable[A]
  ): Encoder[F, Vector[S], OneAnd[G, A]] =
    Encoder.vectorEncoder[F, S, A].contramap(a => a.head +: ev(a.tail).toVector)

  given encodeOneAnd[F[_], S, A, G[_]](
    using
    applicative: Applicative[F],
    encoder: Encoder[F, S, A],
    ev: G[A] => Iterable[A],
    arrayType: ArrayType[S]
  ): Encoder[F, S, OneAnd[G, A]] =
    Encoder.arrayEncoder[F, S, OneAnd[G, A]]

  given vectorEncodeTuple[F[_], S, A <: Tuple](
    using
    applicative: Applicative[F],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, Vector[S], A] with
    def encode(a: A): F[Vector[S]] =
      instances.map[[X] =>> F[S]](a)([T] => (encoder: Encoder[F, S, T], t: T) => encoder.encode(t))
        .toList.asInstanceOf[List[F[S]]].toVector.sequence
  end vectorEncodeTuple

  given encodeTuple[F[_], S, A <: Tuple](
    using
    applicative: Applicative[F],
    arrayType: ArrayType[S],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, S, A] =
    Encoder.arrayEncoder[F, S, A]
end EncoderArrayInstances
