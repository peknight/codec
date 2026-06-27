package com.peknight.codec.error

import com.peknight.error.std.WrongType

object NotObject extends DecodingFailure with WrongType:
  def expectedType: String = "object"
end NotObject
