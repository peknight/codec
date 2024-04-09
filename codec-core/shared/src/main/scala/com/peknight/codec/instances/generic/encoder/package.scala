package com.peknight.codec.instances.generic

package object encoder:
  object all extends EncoderMigrationInstances with EncoderObjectInstances with EncoderArrayInstances
    with EncoderValueInstances
  object migration extends EncoderMigrationInstances
  object obj extends EncoderObjectInstances
  object array extends EncoderArrayInstances
  object value extends EncoderValueInstances

end encoder
