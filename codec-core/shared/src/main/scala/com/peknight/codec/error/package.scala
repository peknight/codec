package com.peknight.codec

import com.peknight.error.Error.{Label, Value}

package object error:
  sealed trait DecodingFailure[A] extends com.peknight.error.codec.DecodingFailure with Value[A]

  sealed trait WrongType[A] extends DecodingFailure[A] with com.peknight.error.std.WrongType:
    def expectedType: String
    override def message: String = s"Got value '$value' with wrong type, expecting $expectedType"
  end WrongType

  case class NotObject[A](value: A) extends WrongType[A]:
    def expectedType: String = "object"
  end NotObject

  case class NonEmptyObject[A](value: A, label: String) extends WrongType[A] with Label:
    def expectedType: String = "non-empty object"
  end NonEmptyObject

  sealed trait StrictDecodingFailure[A] extends DecodingFailure[A] with Label:
    override def message: String = s"Strict decoding $label${lowPriorityMessage.fold("")(msg => s" - $msg")}"
  end StrictDecodingFailure

  case class UnexpectedFields[A](value: A, label: String, unexpectedFields: List[String], expectedFields: List[String])
    extends StrictDecodingFailure[A]:
    override def lowPriorityMessage =
      Some(s"unexpected fields: ${unexpectedFields.mkString(", ")}; valid fields: ${expectedFields.mkString(", ")}.")
  end UnexpectedFields

  case class NotSingleKeyObject[A](value: A, label: String, constructorNames: List[String])
    extends StrictDecodingFailure[A]:
    override def lowPriorityMessage = Some(s"expected a single key object with one of: $constructorNames.")
  end NotSingleKeyObject

  case class NoSuchType[A](value: A, label: String, typeName: String) extends DecodingFailure[A] with Label:
    override def message: String = s"type $label has no class/object/case '$typeName'."
  end NoSuchType

  case class NoDiscriminatorField[A](value: A, label: String, discriminator: String) extends DecodingFailure[A]
    with Label:
    override def message: String = s"$label: could not find discriminator field '$discriminator' or its null.'"
  end NoDiscriminatorField

end error
