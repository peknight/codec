package com.peknight.codec.error

import com.peknight.error.Error.Value

trait DecodingFailure[A] extends com.peknight.error.codec.DecodingFailure with Value[A]
