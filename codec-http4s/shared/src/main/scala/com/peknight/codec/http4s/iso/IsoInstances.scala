package com.peknight.codec.http4s.iso

import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.{Applicative, Id}
import com.peknight.codec.Decoder
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.error.DecodingFailure.Errors
import com.peknight.generic.migration.Isomorphism
import org.http4s.{ParseFailure, QueryParamDecoder, QueryParameterValue}

trait IsoInstances:

  given queryParamDecoderLowPriorityIsomorphism[F[_]: Applicative, A]
  : Isomorphism[F, Decoder[Id, String, A], QueryParamDecoder[A]] =
    queryParamDecoderIsomorphism[F, A](None)

  given decodingFailureLowPriorityIsomorphism[F[_]: Applicative]: Isomorphism[F, DecodingFailure, ParseFailure] =
    decodingFailureIsomorphism[F](None)

  given decodingFailuresLowPriorityIsomorphism[F[_]: Applicative]
  : Isomorphism[F, DecodingFailure, NonEmptyList[ParseFailure]] =
    decodingFailuresIsomorphism[F](None)

  def decodingFailureIsomorphism[F[_]: Applicative](labelOption: Option[String])
  : Isomorphism[F, DecodingFailure, ParseFailure] =
    new Isomorphism[F, DecodingFailure, ParseFailure]:
      def to(a: DecodingFailure): F[ParseFailure] =
        ParseFailure(s"Query decoding ${labelOption.fold("")(label => s"$label ")}failed", a.message).pure
      def from(b: ParseFailure): F[DecodingFailure] = DecodingFailure(b).pure
  end decodingFailureIsomorphism

  def decodingFailuresIsomorphism[F[_]: Applicative](labelOption: Option[String])
  : Isomorphism[F, DecodingFailure, NonEmptyList[ParseFailure]] =
    new Isomorphism[F, DecodingFailure, NonEmptyList[ParseFailure]]:
      def to(a: DecodingFailure): F[NonEmptyList[ParseFailure]] =
        a match
          case Errors(errors) => errors.map(decodingFailureIsomorphism[Id](labelOption).to).pure
          case e => NonEmptyList.one(decodingFailureIsomorphism[Id](labelOption).to(e)).pure
      def from(b: NonEmptyList[ParseFailure]): F[DecodingFailure] = DecodingFailure(b).pure
  end decodingFailuresIsomorphism

  def queryParamDecoderIsomorphism[F[_]: Applicative, A](labelOption: Option[String])
  : Isomorphism[F, Decoder[Id, String, A], QueryParamDecoder[A]] =
    new Isomorphism[F, Decoder[Id, String, A], QueryParamDecoder[A]]:
      def to(a: Decoder[Id, String, A]): F[QueryParamDecoder[A]] =
        val decoder: QueryParamDecoder[A] =
          value => a.decode(value.value).left.map(decodingFailuresIsomorphism[Id](labelOption).to).toValidated
        decoder.pure
      def from(b: QueryParamDecoder[A]): F[Decoder[Id, String, A]] =
        Decoder.applicative[Id, String, A] { t =>
          b.decode(QueryParameterValue(t)).leftMap(decodingFailuresIsomorphism[Id](labelOption).from).toEither
        }.pure
  end queryParamDecoderIsomorphism
end IsoInstances
