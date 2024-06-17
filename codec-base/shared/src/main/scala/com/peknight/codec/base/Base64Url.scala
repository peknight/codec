package com.peknight.codec.base

import scodec.bits.Bases.Base64Alphabet

case class Base64Url private (value: String) extends Base64Base:
  val alphabet: Base64Alphabet = Base64Url.alphabet
end Base64Url
object Base64Url extends BaseAlphabetPlatform[scodec.bits.Bases.Alphabets.Base64Url.type, Base64Url]
  with Base64Platform[scodec.bits.Bases.Alphabets.Base64Url.type, Base64Url]:
  val alphabet: scodec.bits.Bases.Alphabets.Base64Url.type = scodec.bits.Bases.Alphabets.Base64Url
  override protected def toBase: String => Base64Url = Base64Url.apply
end Base64Url

