package com.peknight.codec.error

case class NotSingleKeyObject[A](value: A, label: String, constructorNames: List[String])
  extends StrictDecodingFailure[A]:
  override protected def lowPriorityMessage: Option[String] =
    Some(s"expected a single key object with one of: $constructorNames.")
end NotSingleKeyObject
