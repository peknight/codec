package com.peknight.codec.http4s.instances

import com.peknight.codec.http4s.iso.queryParamEncoderIsomorphism
import com.peknight.codec.id.Encoder
import org.http4s.QueryParamEncoder

trait QueryParamEncoderInstances:
  given queryParamEncoder[A](using encoder: Encoder[String, A]): QueryParamEncoder[A] =
    queryParamEncoderIsomorphism.to(encoder)
end QueryParamEncoderInstances
object QueryParamEncoderInstances extends QueryParamEncoderInstances