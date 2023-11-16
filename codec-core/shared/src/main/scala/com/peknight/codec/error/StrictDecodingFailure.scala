package com.peknight.codec.error

import com.peknight.error.Error.Label

trait StrictDecodingFailure[A] extends DecodingFailure[A] with Label:
  override def message: String = s"Strict decoding $label${lowPriorityMessage.fold("")(msg => s" - $msg")}"
end StrictDecodingFailure
