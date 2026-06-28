package com.peknight.codec.error

import com.peknight.error.std.WrongType

object NotNull extends DecodingFailure with WrongType:
  def expectedType: String = "null"
end NotNull
