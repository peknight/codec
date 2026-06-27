package com.peknight.codec.http4s.circe.instances

import cats.Id
import cats.effect.Concurrent
import com.peknight.codec.Decoder
import com.peknight.codec.circe.iso.decoder
import com.peknight.codec.cursor.Cursor
import io.circe.Json
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf

trait EntityDecoderInstances:
  given jsonDecoderAsEntityDecoder[F[_], A](using concurrent: Concurrent[F], d: Decoder[Id, Cursor[Json], A])
  : EntityDecoder[F, A] =
    jsonOf[F, A](using concurrent, decoder[A])
end EntityDecoderInstances
object EntityDecoderInstances extends EntityDecoderInstances

