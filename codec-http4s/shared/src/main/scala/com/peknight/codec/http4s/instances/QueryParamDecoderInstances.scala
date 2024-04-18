package com.peknight.codec.http4s.instances

import cats.Id
import com.peknight.codec.Decoder
import com.peknight.codec.http4s.iso.queryParamDecoderIsomorphismWithClassTag
import org.http4s.QueryParamDecoder

import scala.reflect.ClassTag

trait QueryParamDecoderInstances extends QueryParamDecoderInstances1:
  given stringDecodeWithClassTagAsQueryParamDecoder[A](using decoder: Decoder[Id, String, A], classTag: ClassTag[A])
  : QueryParamDecoder[A] =
    queryParamDecoderIsomorphismWithClassTag[Id, A].to(decoder)
end QueryParamDecoderInstances
object QueryParamDecoderInstances extends QueryParamDecoderInstances
