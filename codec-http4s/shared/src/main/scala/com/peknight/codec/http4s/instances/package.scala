package com.peknight.codec.http4s

package object instances:
  object all extends QueryParamEncoderInstances with QueryParamDecoderInstances with SegmentEncoderInstances
    with UriInstances
  object queryParamCodec extends QueryParamEncoderInstances with QueryParamDecoderInstances
  object queryParamEncoder extends QueryParamEncoderInstances
  object queryParamDecoder extends QueryParamDecoderInstances
  object segmentEncoder extends SegmentEncoderInstances
  object uri extends UriInstances
end instances
