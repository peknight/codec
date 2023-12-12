package com.peknight.codec.error

object CursorFailure extends DecodingFailure:
    override protected def lowPriorityLabelMessage(label: String): Option[String] = Some(s"Couldn't decode $label")
    override protected def lowPriorityMessage: Option[String] = Some("Couldn't decode")
end CursorFailure