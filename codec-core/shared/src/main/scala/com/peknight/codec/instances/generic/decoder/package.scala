package com.peknight.codec.instances.generic

package object decoder:
  object all extends DecoderEncoderInstances with DecoderObjectInstances with DecoderValueInstances
  object encoder extends DecoderEncoderInstances
  object migration extends DecoderMigrationInstances
  object obj extends DecoderObjectInstances
  object value extends DecoderValueInstances
end decoder
