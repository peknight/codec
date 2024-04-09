package com.peknight.codec.error

import com.peknight.error.std.WrongType

object NotNumber extends DecodingFailure with WrongType:
  def expectedType: String = "number"
end NotNumber
