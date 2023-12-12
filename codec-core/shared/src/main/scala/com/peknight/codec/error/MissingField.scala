package com.peknight.codec.error

object MissingField extends DecodingFailure:
  override protected def lowPriorityLabelMessage(label: String): Option[String] = Some(s"Missing required field: $label")
  override protected def lowPriorityMessage: Option[String] = Some("Missing required field")
end MissingField