package com.peknight.codec.base

import scodec.bits.Bases.Base64Alphabet

case class Base64NoPad(value: String) extends Base64Base:
  val alphabet: Base64Alphabet = Base64NoPad.alphabet
end Base64NoPad
object Base64NoPad extends BaseAlphabetPlatform[scodec.bits.Bases.Alphabets.Base64NoPad.type, Base64NoPad]
  with Base64Platform[scodec.bits.Bases.Alphabets.Base64NoPad.type, Base64NoPad]:
  val alphabet: scodec.bits.Bases.Alphabets.Base64NoPad.type = scodec.bits.Bases.Alphabets.Base64NoPad
  override protected def toBase: String => Base64NoPad = Base64NoPad.apply
end Base64NoPad
