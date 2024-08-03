package com.peknight.codec.circe.parser

import cats.Applicative
import cats.syntax.either.*
import cats.syntax.applicative.*
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.circe.parser.ext.ParserOps.parse
import com.peknight.codec.error.DecodingFailure
import io.circe.Json

object ParserOps:
  def decode[F[_]: Applicative, A](input: String)(using decoder: Decoder[F, Cursor[Json], A])
  : F[Either[DecodingFailure, A]] =
    parse(input) match
      case Left(error) => DecodingFailure(error).asLeft[A].pure[F]
      case Right(json) => decoder.decodeS(json)
end ParserOps
