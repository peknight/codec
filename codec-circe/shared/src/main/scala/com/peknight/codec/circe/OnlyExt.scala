package com.peknight.codec.circe

import cats.{Monad, Show}
import com.peknight.codec.config.CodecConfig
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.*
import com.peknight.codec.{Codec, Decoder, Encoder}
import io.circe.JsonObject

case class OnlyExt(ext: JsonObject = JsonObject.empty) extends Ext
object OnlyExt:
  given codecOnlyExt[F[_], S](using Monad[F], ObjectType[S], NullType[S], ArrayType[S], NumberType[S], BooleanType[S],
    StringType[S], Show[S], Encoder[F, S, JsonObject], Decoder[F, Cursor[S], JsonObject])
  : Codec[F, S, Cursor[S], OnlyExt] =
    given CodecConfig = CodecConfig.default.withExtField("ext")
    Codec.derived[F, S, OnlyExt]
end OnlyExt
