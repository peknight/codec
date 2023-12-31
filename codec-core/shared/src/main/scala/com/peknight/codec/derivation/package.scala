package com.peknight.codec

import com.peknight.codec.instances.{DecoderDerivationInstances, EncoderDerivationInstances}

package object derivation:
  object all extends EncoderDerivationInstances with DecoderDerivationInstances
  object encoder extends EncoderDerivationInstances
  object decoder extends DecoderDerivationInstances
end derivation
