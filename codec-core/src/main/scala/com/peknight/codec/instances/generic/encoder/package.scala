package com.peknight.codec.instances.generic

package object encoder:
  object derivation extends EncoderDerivationInstances
  object migration extends EncoderMigrationInstances
  object obj extends EncoderObjectInstances
  object array extends EncoderArrayInstances
  object value extends EncoderValueInstances
end encoder
