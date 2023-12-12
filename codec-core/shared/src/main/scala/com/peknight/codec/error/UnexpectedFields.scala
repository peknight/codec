package com.peknight.codec.error

case class UnexpectedFields(unexpectedFields: List[String], expectedFields: List[String]) extends StrictDecodingFailure:
  override protected def lowPriorityMessage: Option[String] =
    Some(s"unexpected fields: ${unexpectedFields.mkString(", ")}; valid fields: ${expectedFields.mkString(", ")}.")
end UnexpectedFields
