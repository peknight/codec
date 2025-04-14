package com.peknight.codec.error

object ReadNone extends DecodingFailure:
  override protected def lowPriorityLabelMessage(label: String): Option[String] = Some(s"Read none: $label")
  override protected def lowPriorityMessage: Option[String] = Some("Read none")
end ReadNone
