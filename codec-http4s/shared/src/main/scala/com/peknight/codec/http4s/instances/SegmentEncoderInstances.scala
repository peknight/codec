package com.peknight.codec.http4s.instances

import cats.Id
import com.peknight.codec.Encoder
import com.peknight.codec.http4s.iso.segmentEncoderIsomorphism
import org.http4s.Uri.Path
import org.http4s.Uri.Path.SegmentEncoder


trait SegmentEncoderInstances:
  given stringEncoderAsSegmentEncoder[A](using encoder: Encoder[Id, String, A]): SegmentEncoder[A] =
    segmentEncoderIsomorphism[Id, A].to(encoder)
end SegmentEncoderInstances
object SegmentEncoderInstances extends SegmentEncoderInstances
