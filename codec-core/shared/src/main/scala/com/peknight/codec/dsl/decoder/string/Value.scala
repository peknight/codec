package com.peknight.codec.dsl.decoder.string

import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.id.Decoder

trait Value[A](using Decoder[String, DecodingFailure, A]):
  def unapply(s: String): Option[A] = Value.unapply[A](s)
end Value
object Value:
  def unapply[A](s: String)(using decoder: Decoder[String, DecodingFailure, A]): Option[A] =
    decoder.decode(s).toOption
end Value