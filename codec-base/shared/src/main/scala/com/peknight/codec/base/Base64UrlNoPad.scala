package com.peknight.codec.base

import scodec.bits.Bases.Base64Alphabet

case class Base64UrlNoPad private (value: String) extends Base64Base:
  val alphabet: Base64Alphabet = Base64UrlNoPad.alphabet
end Base64UrlNoPad
object Base64UrlNoPad extends BaseAlphabetPlatform[scodec.bits.Bases.Alphabets.Base64UrlNoPad.type, Base64UrlNoPad]
  with Base64Platform[scodec.bits.Bases.Alphabets.Base64UrlNoPad.type, Base64UrlNoPad]:
  val alphabet: scodec.bits.Bases.Alphabets.Base64UrlNoPad.type = scodec.bits.Bases.Alphabets.Base64UrlNoPad
  override protected def toBase: String => Base64UrlNoPad = Base64UrlNoPad.apply
end Base64UrlNoPad
