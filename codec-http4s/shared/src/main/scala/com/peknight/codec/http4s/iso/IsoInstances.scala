package com.peknight.codec.http4s.iso

import cats.Id
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.id.Decoder
import com.peknight.generic.migration.id.Isomorphism
import org.http4s.{ParseFailure, QueryParamDecoder, QueryParameterValue}

trait IsoInstances:

  given queryParamDecoderLowPriorityIsomorphism[A]
  : Isomorphism[Decoder[String, DecodingFailure, A], QueryParamDecoder[A]] =
    queryParamDecoderIsomorphism(None)

  def queryParamDecoderIsomorphism[A](labelOption: Option[String])
  : Isomorphism[Decoder[String, DecodingFailure, A], QueryParamDecoder[A]] =
    new Isomorphism[Decoder[String, DecodingFailure, A], QueryParamDecoder[A]]:
      def to(a: Decoder[String, DecodingFailure, A]): Id[QueryParamDecoder[A]] =
        value => a.decodeAccumulating(value.value).leftMap(_.map(error =>
          ParseFailure(s"Query decoding ${labelOption.fold("")(label => s"$label ")}failed", error.message)
        ))
      def from(b: QueryParamDecoder[A]): Id[Decoder[String, DecodingFailure, A]] =
        com.peknight.codec.Decoder.instance[Id, String, DecodingFailure, A] { t =>
          b.decode(QueryParameterValue(t)).leftMap(DecodingFailure.apply).toEither
        }
  end queryParamDecoderIsomorphism
end IsoInstances
