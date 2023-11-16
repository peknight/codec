package com.peknight.codec.error

trait WrongType[A] extends DecodingFailure[A] with com.peknight.error.std.WrongType:
  def expectedType: String
  override def message: String = s"Got value '$value' with wrong type, expecting $expectedType"
end WrongType
