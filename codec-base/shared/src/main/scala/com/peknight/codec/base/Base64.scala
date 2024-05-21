package com.peknight.codec.base

import scodec.bits.Bases.Base64Alphabet

case class Base64(value: String) extends Base64Base:
  val alphabet: Base64Alphabet = Base64.alphabet
end Base64
object Base64 extends BaseAlphabetPlatform[scodec.bits.Bases.Alphabets.Base64.type, Base64]
  with Base64Platform[scodec.bits.Bases.Alphabets.Base64.type, Base64]:
  val alphabet: scodec.bits.Bases.Alphabets.Base64.type = scodec.bits.Bases.Alphabets.Base64
  override protected def toBase: String => Base64 = Base64.apply
end Base64
