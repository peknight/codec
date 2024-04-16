package com.peknight.codec.http4s.instances

import com.peknight.codec.http4s.iso.segmentEncoderIsomorphism
import com.peknight.codec.id.Encoder
import org.http4s.Uri.Path
import org.http4s.Uri.Path.SegmentEncoder


trait SegmentEncoderInstances:
  given segmentEncoder[A](using encoder: Encoder[String, A]): SegmentEncoder[A] =
    segmentEncoderIsomorphism.to(encoder)
end SegmentEncoderInstances
object SegmentEncoderInstances extends SegmentEncoderInstances
