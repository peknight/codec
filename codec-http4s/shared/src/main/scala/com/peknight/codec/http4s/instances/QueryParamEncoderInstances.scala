package com.peknight.codec.http4s.instances

import cats.Id
import com.peknight.codec.Encoder
import com.peknight.codec.http4s.iso.queryParamEncoderIsomorphism
import org.http4s.QueryParamEncoder

trait QueryParamEncoderInstances:
  given stringEncoderAsQueryParamEncoder[A](using encoder: Encoder[Id, String, A]): QueryParamEncoder[A] =
    queryParamEncoderIsomorphism[Id, A].to(encoder)
end QueryParamEncoderInstances
object QueryParamEncoderInstances extends QueryParamEncoderInstances