package com.peknight.codec.error

import com.peknight.codec.cursor.FailedCursor
import com.peknight.error.Error
import com.peknight.error.Error.Label

case class MissingField[S](value: FailedCursor[S]) extends CursorDecodingFailure[S]:
  override def lowPriorityMessage: Option[String] = Some("Missing required field")
  override def lowPriorityLabelMessage(label: String): Option[String] = Some(s"Missing required field: $label")
end MissingField
