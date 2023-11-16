package com.peknight.codec.error

import com.peknight.error.Error.Label

case class NoDiscriminatorField[A](value: A, label: String, discriminator: String) extends DecodingFailure[A]
  with Label:
  override def message: String = s"$label: could not find discriminator field '$discriminator' or its null.'"
end NoDiscriminatorField

