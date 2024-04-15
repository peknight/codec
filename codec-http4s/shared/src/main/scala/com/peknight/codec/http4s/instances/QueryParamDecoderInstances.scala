package com.peknight.codec.http4s.instances

import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.http4s.iso.queryParamDecoderClassTagIsomorphism
import com.peknight.codec.id.Decoder
import org.http4s.QueryParamDecoder

import scala.reflect.ClassTag

trait QueryParamDecoderInstances extends QueryParamDecoderInstances1:
  given queryParamDecoderClassTag[A](using decoder: Decoder[String, DecodingFailure, A], classTag: ClassTag[A])
  : QueryParamDecoder[A] =
    queryParamDecoderClassTagIsomorphism.to(decoder)
end QueryParamDecoderInstances
object QueryParamDecoderInstances extends QueryParamDecoderInstances
