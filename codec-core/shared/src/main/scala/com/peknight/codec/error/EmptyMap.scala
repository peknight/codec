package com.peknight.codec.error

import com.peknight.error.std.WrongType

object EmptyMap extends DecodingFailure with WrongType:
  def expectedType: String = "NonEmptyMap"
end EmptyMap
