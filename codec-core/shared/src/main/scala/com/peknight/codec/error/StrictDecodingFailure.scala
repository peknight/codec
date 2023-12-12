package com.peknight.codec.error

trait StrictDecodingFailure extends DecodingFailure:
  override protected def lowPriorityLabelMessage(label: String): Option[String] =
    Some(s"Strict decoding $label${lowPriorityMessage.fold("")(msg => s" - $msg")}")
end StrictDecodingFailure
