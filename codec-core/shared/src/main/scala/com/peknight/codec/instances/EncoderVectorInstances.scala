package com.peknight.codec.instances

import cats.Applicative
import cats.data.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import com.peknight.codec.Encoder
import com.peknight.generic.Generic
import com.peknight.generic.priority.HighPriority

trait EncoderVectorInstances:
  given encodeVectorSeq[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Seq[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeVectorSet[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Set[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeVectorList[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], List[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeVectorVector[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Vector[A]] =
    Encoder.vectorEncoder[F, S, A]

  given encodeVectorChain[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Chain[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeVectorNonEmptyList[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], NonEmptyList[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toList.toVector)

  given encodeVectorNonEmptyVector[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], NonEmptyVector[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeVectorNonEmptySet[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], NonEmptySet[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toSortedSet.toVector)

  given encodeVectorNonEmptyChain[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], NonEmptyChain[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toChain.toVector)

  given encodeVectorOneAnd[F[_], S, A, G[_]](using applicative: Applicative[F], encoder: Encoder[F, S, A],
                                       ev: G[A] => Iterable[A]): Encoder[F, Vector[S], OneAnd[G, A]] =
    Encoder.vectorEncoder[F, S, A].contramap(a => a.head +: ev(a.tail).toVector)

  given encodeVectorIterable[F[_], S, A, G[_]](using applicative: Applicative[F], encoder: Encoder[F, S, A],
                                         ev: G[A] => Iterable[A]): HighPriority[Encoder[F, Vector[S], G[A]]] =
    HighPriority(Encoder.vectorEncoder[F, S, A].contramap(a => ev(a).toVector))

  given encodeVectorTuple[F[_], S, A <: Tuple](using applicative: Applicative[F],
                                         instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A])
  : Encoder[F, Vector[S], A] with
    def encode(a: A): F[Vector[S]] =
      instances.map[[X] =>> F[S]](a)([T] => (encoder: Encoder[F, S, T], t: T) => encoder.encode(t))
        .toList.asInstanceOf[List[F[S]]].toVector.sequence
  end encodeVectorTuple
end EncoderVectorInstances
