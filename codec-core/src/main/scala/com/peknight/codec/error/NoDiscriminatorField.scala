package com.peknight.codec.error

case class NoDiscriminatorField(discriminator: String) extends DecodingFailure:
  override protected def lowPriorityMessage: Option[String] =
    Some(s"could not find discriminator field '$discriminator' or its null.'")
end NoDiscriminatorField
