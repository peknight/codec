package com.peknight.codec.base

import scodec.bits.Bases.Base32Alphabet

case class Base32Crockford private (value: String) extends Base32Base:
  val alphabet: Base32Alphabet = Base32Crockford.alphabet
end Base32Crockford
object Base32Crockford extends BaseAlphabetPlatform[scodec.bits.Bases.Alphabets.Base32Crockford.type, Base32Crockford]
  with Base32Platform[scodec.bits.Bases.Alphabets.Base32Crockford.type, Base32Crockford]:
  val alphabet: scodec.bits.Bases.Alphabets.Base32Crockford.type = scodec.bits.Bases.Alphabets.Base32Crockford
  override protected def toBase: String => Base32Crockford = Base32Crockford.apply
end Base32Crockford
