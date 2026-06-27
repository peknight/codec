package com.peknight.codec

import com.peknight.codec.instances.generic.decoder.DecoderDerivationInstances
import com.peknight.codec.instances.generic.encoder.EncoderDerivationInstances

package object derivation:
  object all extends EncoderDerivationInstances with DecoderDerivationInstances
  object encoder extends EncoderDerivationInstances
  object decoder extends DecoderDerivationInstances
end derivation
