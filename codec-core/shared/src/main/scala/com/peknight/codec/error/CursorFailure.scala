package com.peknight.codec.error

import com.peknight.codec.cursor.Cursor

case class CursorFailure[A](value: A) extends DecodingFailure[A]:
  def map[B](f: A => B): DecodingFailure[B] = CursorFailure(f(value))
  override def lowPriorityLabelMessage(label: String): Option[String] = Some(s"Couldn't decode $label")
  override def lowPriorityMessage: Option[String] = Some("Couldn't decode")
end CursorFailure
