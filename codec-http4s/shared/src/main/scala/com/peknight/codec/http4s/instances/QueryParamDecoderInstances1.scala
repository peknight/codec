package com.peknight.codec.http4s.instances

import cats.Id
import com.peknight.codec.Decoder
import com.peknight.codec.http4s.iso.queryParamDecoderLowPriorityIsomorphism
import org.http4s.QueryParamDecoder

trait QueryParamDecoderInstances1:
  given stringDecoderAsQueryParamDecoder[A](using decoder: Decoder[Id, String, A])
  : QueryParamDecoder[A] =
    queryParamDecoderLowPriorityIsomorphism[Id, A].to(decoder)
end QueryParamDecoderInstances1
