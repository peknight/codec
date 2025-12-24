package com.peknight.codec.circe

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.circe.parser.parse
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import io.circe.Json

package object parser:
  def decode[F[_]: Applicative, A](input: String)(using decoder: Decoder[F, Cursor[Json], A])
  : F[Either[DecodingFailure, A]] =
    parse(input) match
      case Left(error) => DecodingFailure(error).asLeft[A].pure[F]
      case Right(json) => decoder.decodeS(json)
end parser
