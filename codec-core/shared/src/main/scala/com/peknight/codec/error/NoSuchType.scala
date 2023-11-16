package com.peknight.codec.error

import com.peknight.error.Error.Label

case class NoSuchType[A](value: A, label: String, typeName: String) extends DecodingFailure[A] with Label:
  override def message: String = s"type $label has no class/object/case '$typeName'."
end NoSuchType
