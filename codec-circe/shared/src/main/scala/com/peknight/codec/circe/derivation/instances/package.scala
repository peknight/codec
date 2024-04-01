package com.peknight.codec.circe.derivation

package object instances:
  object all extends EncoderDerivationInstances with DecoderDerivationInstances
  object encoder extends EncoderDerivationInstances
  object decoder extends DecoderDerivationInstances
end instances
