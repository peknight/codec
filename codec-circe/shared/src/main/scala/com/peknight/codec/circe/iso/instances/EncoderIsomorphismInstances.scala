package com.peknight.codec.circe.iso.instances

import com.peknight.codec.circe.iso.migrateEncoder
import com.peknight.codec.id.Encoder
import io.circe.Json
import io.circe.`export`.Exported

trait EncoderIsomorphismInstances:
  given encoderIsomorphism[A](using encoder: Encoder[Json, A]): Exported[io.circe.Encoder[A]] =
    Exported(migrateEncoder(encoder))
end EncoderIsomorphismInstances
object EncoderIsomorphismInstances extends EncoderIsomorphismInstances