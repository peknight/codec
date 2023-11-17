package com.peknight.codec.error

import com.peknight.error.Error.Label

case class NoSuchEnum[A](value: A, label: String, caseName: String) extends DecodingFailure[A] with Label:
  override def message: String = s"enum $label does not contain case: $caseName"
end NoSuchEnum
