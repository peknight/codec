package com.peknight.codec.error

object NoDiscriminator extends DecodingFailure:
  override protected def lowPriorityMessage: Option[String] = Some("No discriminator")
end NoDiscriminator
