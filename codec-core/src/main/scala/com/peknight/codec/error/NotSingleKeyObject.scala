package com.peknight.codec.error

case class NotSingleKeyObject(constructorNames: List[String]) extends StrictDecodingFailure:
  override protected def lowPriorityMessage: Option[String] =
    Some(s"expected a single key object with one of: $constructorNames.")
end NotSingleKeyObject
