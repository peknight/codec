package com.peknight.codec.http4s

package object instances:
  object queryParamCodec extends QueryParamEncoderInstances with QueryParamDecoderInstances
  object queryParamEncoder extends QueryParamEncoderInstances
  object queryParamDecoder extends QueryParamDecoderInstances
  object segmentEncoder extends SegmentEncoderInstances
  object status extends StatusInstances
  object uri extends UriInstances
end instances
