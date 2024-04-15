package com.peknight.codec.http4s

import cats.Id
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.id.{Decoder, Encoder}
import com.peknight.error.Error
import com.peknight.generic.migration.id.Isomorphism
import org.http4s.{QueryParamDecoder, QueryParamEncoder, QueryParameterValue}

import scala.reflect.ClassTag

package object iso extends IsoInstances:
  given queryParamDecoderClassTagIsomorphism[A](using ClassTag[A])
  : Isomorphism[Decoder[String, DecodingFailure, A], QueryParamDecoder[A]] =
    queryParamDecoderIsomorphism(Some(Error.errorClassTag[A]))

  given queryParamEncoderIsomorphism[A]: Isomorphism[Encoder[String, A], QueryParamEncoder[A]] with
    def to(a: Encoder[String, A]): Id[QueryParamEncoder[A]] =
      (value: A) => QueryParameterValue(a.encode(value))
    def from(b: QueryParamEncoder[A]): Id[Encoder[String, A]] =
      com.peknight.codec.Encoder.instance[Id, String, A](a => b.encode(a).value)
  end queryParamEncoderIsomorphism

end iso
