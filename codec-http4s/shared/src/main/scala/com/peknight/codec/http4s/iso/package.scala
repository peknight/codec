package com.peknight.codec.http4s

import cats.syntax.applicative.*
import cats.{Applicative, Id}
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.{Decoder, Encoder}
import com.peknight.error.Error
import com.peknight.generic.migration.Isomorphism
import org.http4s.Uri.Path
import org.http4s.Uri.Path.{Segment, SegmentEncoder}
import org.http4s.{ParseFailure, QueryParamDecoder, QueryParamEncoder, QueryParameterValue}

import scala.reflect.ClassTag

package object iso extends IsoInstances:
  given queryParamDecoderIsomorphismWithClassTag[F[_]: Applicative, A: ClassTag]
  : Isomorphism[F, Decoder[Id, String, A], QueryParamDecoder[A]] =
    queryParamDecoderIsomorphism0(Some(Error.errorClassTag[A]))

  given queryParamEncoderIsomorphism[F[_]: Applicative, A]: Isomorphism[F, Encoder[Id, String, A], QueryParamEncoder[A]] with
    def to(a: Encoder[Id, String, A]): F[QueryParamEncoder[A]] =
      val encoder: QueryParamEncoder[A] = (value: A) => QueryParameterValue(a.encode(value))
      encoder.pure
    def from(b: QueryParamEncoder[A]): F[Encoder[Id, String, A]] =
      Encoder.applicative[Id, String, A](a => b.encode(a).value).pure
  end queryParamEncoderIsomorphism

  given segmentEncoderIsomorphism[F[_]: Applicative, A]: Isomorphism[F, Encoder[Id, String, A], SegmentEncoder[A]] with
    def to(encoder: Encoder[Id, String, A]): F[SegmentEncoder[A]] =
      val segmentEncoder: SegmentEncoder[A] = (a: A) => Segment(encoder.encode(a))
      segmentEncoder.pure
    def from(encoder: SegmentEncoder[A]): F[Encoder[Id, String, A]] =
      val segmentEncoder: Encoder[Id, String, A] = (a: A) => encoder.toSegment(a).decoded()
      segmentEncoder.pure
  end segmentEncoderIsomorphism

end iso
