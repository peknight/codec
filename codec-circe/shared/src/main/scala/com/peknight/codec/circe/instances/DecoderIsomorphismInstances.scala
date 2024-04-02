package com.peknight.codec.circe.instances

import com.peknight.codec.circe.iso.decoder
import com.peknight.codec.circe.sum.JsonTypeInstances
import com.peknight.codec.cursor.id.Decoder
import io.circe.Json
import io.circe.`export`.Exported

trait DecoderIsomorphismInstances extends JsonTypeInstances:
  given decoderIsomorphism[A](using Decoder[Json, A]): Exported[io.circe.Decoder[A]] = Exported(decoder)
end DecoderIsomorphismInstances
object DecoderIsomorphismInstances extends DecoderIsomorphismInstances