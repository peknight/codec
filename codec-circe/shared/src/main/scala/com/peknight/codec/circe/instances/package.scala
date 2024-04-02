package com.peknight.codec.circe

package object instances:
  object all extends EncoderIsomorphismInstances with DecoderIsomorphismInstances
  object encoder extends EncoderIsomorphismInstances
  object decoder extends DecoderIsomorphismInstances
end instances
