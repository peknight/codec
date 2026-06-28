package com.peknight.codec.error

import com.peknight.error.std.WrongType

object NonEmptyObject extends DecodingFailure with WrongType:
  def expectedType: String = "non-empty object"
end NonEmptyObject
