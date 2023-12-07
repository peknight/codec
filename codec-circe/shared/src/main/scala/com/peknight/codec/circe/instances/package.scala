package com.peknight.codec.circe

import com.peknight.codec.circe.instances.{DecoderDerivationInstances, EncoderDerivationInstances}

package object instances:
  object all extends EncoderDerivationInstances with DecoderDerivationInstances
  object encoder extends EncoderDerivationInstances
  object decoder extends DecoderDerivationInstances
end instances
