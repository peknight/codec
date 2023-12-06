package com.peknight.codec.instances

import cats.data.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Applicative, Functor}
import com.peknight.codec.Encoder
import com.peknight.codec.sum.ArrayType
import com.peknight.generic.Generic
import com.peknight.generic.priority.MidPriority

trait EncoderVectorInstances:
  given encodeSeq[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Seq[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeSet[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Set[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeList[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], List[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeVector[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Vector[A]] =
    Encoder.vectorEncoder[F, S, A]

  given encodeChain[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Chain[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeNonEmptyList[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], NonEmptyList[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toList.toVector)

  given encodeNonEmptyVector[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], NonEmptyVector[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toVector)

  given encodeNonEmptySet[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], NonEmptySet[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toSortedSet.toVector)

  given encodeNonEmptyChain[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], NonEmptyChain[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(_.toChain.toVector)

  given encodeOneAnd[F[_], S, A, G[_]](using applicative: Applicative[F], encoder: Encoder[F, S, A],
                                       ev: G[A] => Iterable[A]): Encoder[F, Vector[S], OneAnd[G, A]] =
    Encoder.vectorEncoder[F, S, A].contramap(a => a.head +: ev(a.tail).toVector)

  given encodeIterable[F[_], S, A, G[_]](using applicative: Applicative[F], encoder: Encoder[F, S, A],
                                         ev: G[A] => Iterable[A]): MidPriority[Encoder[F, Vector[S], G[A]]] =
    MidPriority(Encoder.vectorEncoder[F, S, A].contramap(a => ev(a).toVector))

  given encodeTuple[F[_], S, A <: Tuple](using applicative: Applicative[F],
                                         instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A])
  : Encoder[F, Vector[S], A] with
    def encode(a: A): F[Vector[S]] =
      instances.map[[X] =>> F[S]](a)([T] => (encoder: Encoder[F, S, T], t: T) => encoder.encode(t))
        .toList.asInstanceOf[List[F[S]]].toVector.sequence
  end encodeTuple

  given vectorEncoder[F[_], S, A](using functor: Functor[F], encoder: Encoder[F, Vector[S], A], arrayType: ArrayType[S])
  : Encoder[F, S, A] with
    def encode(a: A): F[S] = encoder.encode(a).map(arr => arrayType.to(arrayType.fromArray(arr)))
  end vectorEncoder

end EncoderVectorInstances
