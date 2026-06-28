package com.peknight.codec.http4s.instances

import cats.{Applicative, Show}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
import com.peknight.codec.{Codec, Decoder, Encoder}
import org.http4s.Uri.Path

import scala.util.Try

trait PathInstances:
  given stringCodecPath[F[_]: Applicative]: Codec[F, String, String, Path] =
    Codec.mapTry[F, String, String, Path](_.toString)(t => Try(Path.unsafeFromString(t)))
  given encodePathS[F[_]: Applicative, S: StringType]: Encoder[F, S, Path] = Encoder.encodeS[F, S, Path]
  given decodePathS[F[_]: Applicative, S: {StringType, Show}]: Decoder[F, Cursor[S], Path] = Decoder.decodeS[F, S, Path]
end PathInstances
object PathInstances extends PathInstances
