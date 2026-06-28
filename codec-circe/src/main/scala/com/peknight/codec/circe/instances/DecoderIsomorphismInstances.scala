package com.peknight.codec.circe.instances

import cats.Id
import com.peknight.codec.Decoder
import com.peknight.codec.circe.iso.decoder
import com.peknight.codec.circe.sum.JsonTypeInstances
import com.peknight.codec.cursor.Cursor
import io.circe.Json
import io.circe.`export`.Exported

trait DecoderIsomorphismInstances extends JsonTypeInstances:
  given decoderIsomorphism[A](using Decoder[Id, Cursor[Json], A]): Exported[io.circe.Decoder[A]] = Exported(decoder)
end DecoderIsomorphismInstances
object DecoderIsomorphismInstances extends DecoderIsomorphismInstances