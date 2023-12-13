package com.peknight.codec.error

case class TupleSizeNotMatch(expectedSize: Int, actualSize: Int) extends StrictDecodingFailure:
  override protected def lowPriorityMessage: Option[String] =
    Some(s"expected tuple size: $expectedSize, but was $actualSize.")
end TupleSizeNotMatch
