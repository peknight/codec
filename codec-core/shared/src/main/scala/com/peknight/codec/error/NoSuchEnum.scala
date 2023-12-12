package com.peknight.codec.error

case class NoSuchEnum(typeName: String) extends NoSuchType:
  override protected def lowPriorityLabelMessage(label: String): Option[String] =
    Some(s"enum $label does not contain case: $typeName")
end NoSuchEnum
