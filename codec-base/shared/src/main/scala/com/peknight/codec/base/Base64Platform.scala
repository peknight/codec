package com.peknight.codec.base

import scodec.bits.Bases.Base64Alphabet
import scodec.bits.ByteVector

trait Base64Platform[A <: Base64Alphabet, B <: Base64Base] extends BasePlatform[A, B]:
  protected def fromBaseDescriptiveWithAlphabet: (String, A) => Either[String, ByteVector] = ByteVector.fromBase64Descriptive
  protected def toBaseStringWithAlphabet: (ByteVector, A) => String = (vector, alphabet) => vector.toBase64(alphabet)
  protected def maxPadLength: Int = 3
end Base64Platform
