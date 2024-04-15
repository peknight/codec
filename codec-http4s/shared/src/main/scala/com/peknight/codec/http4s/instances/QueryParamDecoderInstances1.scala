package com.peknight.codec.http4s.instances

import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.http4s.iso.queryParamDecoderLowPriorityIsomorphism
import com.peknight.codec.id.Decoder
import org.http4s.QueryParamDecoder

trait QueryParamDecoderInstances1:
  given queryParamDecoderLowPriority[A](using decoder: Decoder[String, DecodingFailure, A])
  : QueryParamDecoder[A] =
    queryParamDecoderLowPriorityIsomorphism.to(decoder)
end QueryParamDecoderInstances1
