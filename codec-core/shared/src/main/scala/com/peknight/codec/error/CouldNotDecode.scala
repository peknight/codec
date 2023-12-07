package com.peknight.codec.error

import com.peknight.codec.cursor.Cursor

case class CouldNotDecode[A](value: A) extends DecodingFailure[A]:
  override def lowPriorityLabelMessage(label: String): Option[String] = Some(s"Couldn't decode $label")
  override def lowPriorityMessage: Option[String] = Some("Couldn't decode")
end CouldNotDecode
