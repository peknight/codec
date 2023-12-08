package com.peknight.codec.error

import com.peknight.error.Error.Label

case class NoDiscriminatorField[A](value: A, label: String, discriminator: String) extends DecodingFailure[A]
  with Label:
  def map[B](f: A => B): DecodingFailure[B] = NoDiscriminatorField(f(value), label, discriminator)
  override def lowPriorityLabelMessage(label: String): Option[String] =
    Some(s"$label: could not find discriminator field '$discriminator' or its null.'")
end NoDiscriminatorField
