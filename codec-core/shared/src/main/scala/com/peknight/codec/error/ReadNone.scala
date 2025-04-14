package com.peknight.codec.error

import com.peknight.codec.reader.Key

case class ReadNone(key: Key) extends DecodingFailure:
  override protected def lowPriorityMessage: Option[String] = Some(s"Read none: ${key.keys.mkString(".")}")
end ReadNone
