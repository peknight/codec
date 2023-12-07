package com.peknight.codec.error

import com.peknight.codec.cursor.Cursor
import com.peknight.error.Error.Label

case class CouldNotDecode[S](value: Cursor[S]) extends CursorDecodingFailure[S] with Label:
  override def label: String = value.pathString.replaceFirst("^\\.", "")
  override def labelOption: Option[String] = Some(label)
  override def lowPriorityLabelMessage(label: String): Option[String] = Some(s"Couldn't decode $label")
  override def lowPriorityMessage: Option[String] = Some("Couldn't decode")
end CouldNotDecode
