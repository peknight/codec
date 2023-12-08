package com.peknight.codec.error

import com.peknight.error.Error.Label

case class NoSuchEnum[A](value: A, label: String, caseName: String) extends DecodingFailure[A] with Label:
  def map[B](f: A => B): DecodingFailure[B] = NoSuchEnum(f(value), label, caseName)
  override def lowPriorityLabelMessage(label: String): Option[String] =
    Some(s"enum $label does not contain case: $caseName")
end NoSuchEnum
