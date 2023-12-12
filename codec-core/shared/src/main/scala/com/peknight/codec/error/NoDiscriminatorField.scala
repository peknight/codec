package com.peknight.codec.error

case class NoDiscriminatorField(discriminator: String) extends DecodingFailure:
  override protected def lowPriorityLabelMessage(label: String): Option[String] =
    Some(s"$label: could not find discriminator field '$discriminator' or its null.'")
end NoDiscriminatorField
