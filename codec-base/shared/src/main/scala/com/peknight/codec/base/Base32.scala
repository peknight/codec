package com.peknight.codec.base

import scodec.bits.Bases.Base32Alphabet

case class Base32 private (value: String) extends Base32Base:
  val alphabet: Base32Alphabet = Base32.alphabet
end Base32
object Base32 extends BaseAlphabetPlatform[scodec.bits.Bases.Alphabets.Base32.type, Base32]
  with Base32Platform[scodec.bits.Bases.Alphabets.Base32.type, Base32]:
  val alphabet: scodec.bits.Bases.Alphabets.Base32.type = scodec.bits.Bases.Alphabets.Base32
  override protected def toBase: String => Base32 = Base32.apply
end Base32
