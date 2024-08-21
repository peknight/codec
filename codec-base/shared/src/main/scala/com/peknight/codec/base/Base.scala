package com.peknight.codec.base

import cats.Applicative
import cats.syntax.functor.*
import com.peknight.codec.error.DecodingFailure
import scodec.bits.Bases.Alphabet
import scodec.bits.ByteVector
import com.peknight.commons.bigint.syntax.byteVector.toUnsignedBigInt

trait Base:
  def value: String
  def alphabet: Alphabet
  def decode[F[_]: Applicative]: F[Either[DecodingFailure, ByteVector]]
  def decodeToUnsignedBigInt[F[_]: Applicative]: F[Either[DecodingFailure, BigInt]] = decode[F].map(_.map(_.toUnsignedBigInt))
  def unsafeDecode[F[_]: Applicative]: F[ByteVector] = decode[F].map(_.fold(throw _, identity))
  def unsafeDecodeToUnsignedBigInt[F[_]: Applicative]: F[BigInt] = decodeToUnsignedBigInt[F].map(_.fold(throw _, identity))
end Base
