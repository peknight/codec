package com.peknight.codec.http4s.circe

package object instances:
  object all extends EntityEncoderInstances with EntityDecoderInstances
  object entityEncoder extends EntityEncoderInstances
  object entityDecoder extends EntityDecoderInstances
end instances
