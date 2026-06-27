package com.peknight.codec.error

trait NoSuchType extends DecodingFailure:
  def typeName: String
  override protected def lowPriorityLabelMessage(label: String): Option[String] =
    Some(s"type $label has no class/object/case '$typeName'.")
end NoSuchType
object NoSuchType:
  private case class NoSuchType(typeName: String) extends com.peknight.codec.error.NoSuchType
  def apply(typeName: String): com.peknight.codec.error.NoSuchType = NoSuchType(typeName)
end NoSuchType
