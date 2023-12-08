package com.peknight.codec.error

import com.peknight.error.Error.Label

case class NoSuchType[A](value: A, label: String, typeName: String) extends DecodingFailure[A] with Label:
  def map[B](f: A => B): DecodingFailure[B] = NoSuchType(f(value), label, typeName)
  override def lowPriorityLabelMessage(label: String): Option[String] =
    Some(s"type $label has no class/object/case '$typeName'.")
end NoSuchType
