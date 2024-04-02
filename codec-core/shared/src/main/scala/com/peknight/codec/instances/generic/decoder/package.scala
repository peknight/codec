package com.peknight.codec.instances.generic

package object decoder:
  object all extends DecoderEncoderInstances with DecoderObjectInstances with DecoderStringInstances
  object encoder extends DecoderEncoderInstances
  object migration extends DecoderMigrationInstances
  object obj extends DecoderObjectInstances
  object string extends DecoderStringInstances
end decoder
