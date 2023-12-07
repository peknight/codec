package com.peknight.codec.error

case class MissingField[A](value: A) extends DecodingFailure[A]:
  override def lowPriorityMessage: Option[String] = Some("Missing required field")
  override def lowPriorityLabelMessage(label: String): Option[String] = Some(s"Missing required field: $label")
end MissingField
