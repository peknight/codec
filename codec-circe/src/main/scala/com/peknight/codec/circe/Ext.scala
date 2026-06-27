package com.peknight.codec.circe

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import io.circe.{Json, JsonObject}

trait Ext:
  def ext: JsonObject
  def decodeExt[F[_], A](using applicative: Applicative[F], decoder: Decoder[F, Cursor[Json], A])
  : F[Either[DecodingFailure, A]] =
    decoder.decodeS(Json.fromJsonObject(ext))
end Ext
