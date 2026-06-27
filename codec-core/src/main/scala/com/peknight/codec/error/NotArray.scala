package com.peknight.codec.error

import com.peknight.error.std.WrongType

object NotArray extends DecodingFailure with WrongType:
  def expectedType: String = "array"
end NotArray
