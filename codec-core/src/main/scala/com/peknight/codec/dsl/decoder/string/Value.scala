package com.peknight.codec.dsl.decoder.string

import cats.Id
import com.peknight.codec.Decoder

trait Value[A](using Decoder[Id, String, A]):
  def unapply(s: String): Option[A] = Value.unapply[A](s)
end Value
object Value:
  def unapply[A](s: String)(using decoder: Decoder[Id, String, A]): Option[A] =
    decoder.decode(s).toOption
end Value