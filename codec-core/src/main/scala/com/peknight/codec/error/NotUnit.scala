package com.peknight.codec.error

import com.peknight.error.std.WrongType

object NotUnit extends DecodingFailure with WrongType:
  def expectedType: String = "'null' or '[]' or '{}'"
end NotUnit
