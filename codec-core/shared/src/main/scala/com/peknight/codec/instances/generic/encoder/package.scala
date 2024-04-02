package com.peknight.codec.instances.generic

package object encoder:
  object all extends EncoderMigrationInstances with EncoderObjectInstances with EncoderArrayInstances
    with EncoderStringInstances
  object migration extends EncoderMigrationInstances
  object obj extends EncoderObjectInstances
  object array extends EncoderArrayInstances
  object string extends EncoderStringInstances

end encoder
