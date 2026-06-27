package com.peknight.codec.http4s.circe.instances

import cats.Id
import com.peknight.codec.Encoder
import com.peknight.codec.circe.iso.encoder
import io.circe.Json
import org.http4s.EntityEncoder
import org.http4s.circe.jsonEncoderOf

trait EntityEncoderInstances:
  given jsonEncoderAsEntityEncoder[A](using e: Encoder[Id, Json, A]): EntityEncoder.Pure[A] =
    jsonEncoderOf[A](using encoder[A])
end EntityEncoderInstances
object EntityEncoderInstances extends EntityEncoderInstances

