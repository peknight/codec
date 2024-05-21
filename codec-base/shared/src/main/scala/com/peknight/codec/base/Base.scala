package com.peknight.codec.base

import cats.Applicative
import cats.syntax.functor.*
import com.peknight.codec.error.DecodingFailure
import scodec.bits.Bases.Alphabet
import scodec.bits.ByteVector

trait Base:
  def value: String
  def alphabet: Alphabet
  def decode[F[_]: Applicative]: F[Either[DecodingFailure, ByteVector]]
  def unsafeDecode[F[_]: Applicative]: F[ByteVector] = decode[F].map(_.fold(throw _, identity))
end Base
