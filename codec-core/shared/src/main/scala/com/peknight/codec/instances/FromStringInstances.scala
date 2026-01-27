package com.peknight.codec.instances

import cats.Id
import com.peknight.codec.Decoder

import scala.util.CommandLineParser.FromString

trait FromStringInstances:
  given [A](using decoder: Decoder[Id, String, A]): FromString[A] with
    def fromString(s: String): A =
      decoder.decode(s) match
        case Right(a) => a
        case Left(e) => throw new IllegalArgumentException(e.getMessage)
  end given
end FromStringInstances
object FromStringInstances extends FromStringInstances
