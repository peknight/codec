package com.peknight.codec.base

import scodec.bits.Bases.Base32Alphabet
import scodec.bits.ByteVector

trait Base32Platform[A <: Base32Alphabet, B <: Base32Base] extends BasePlatform[A, B]:
  protected def fromBaseDescriptiveWithAlphabet: (String, A) => Either[String, ByteVector] = ByteVector.fromBase32Descriptive
  protected def toBaseStringWithAlphabet: (ByteVector, A) => String = (vector, alphabet) => vector.toBase32(alphabet)
  protected def maxPadLength: Int = 6
end Base32Platform
