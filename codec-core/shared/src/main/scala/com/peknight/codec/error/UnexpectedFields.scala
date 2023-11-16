package com.peknight.codec.error

case class UnexpectedFields[A](value: A, label: String, unexpectedFields: List[String], expectedFields: List[String])
  extends StrictDecodingFailure[A]:
  override protected def lowPriorityMessage: Option[String] =
    Some(s"unexpected fields: ${unexpectedFields.mkString(", ")}; valid fields: ${expectedFields.mkString(", ")}.")
end UnexpectedFields
