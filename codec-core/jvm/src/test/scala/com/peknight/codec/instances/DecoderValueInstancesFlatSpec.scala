package com.peknight.codec.instances

import cats.Id
import com.peknight.codec.Decoder
import org.scalatest.flatspec.AnyFlatSpec

import java.time.Instant

class DecoderValueInstancesFlatSpec extends AnyFlatSpec:
  "Instant Decoder" should "succeed" in {
    assert(Decoder[Id, String, Instant].decode("2011-12-03T10:15:30Z").isRight)
    assert(Decoder[Id, String, Instant].decode("2021-06-10T15:17:27.987721Z").isRight)
  }
end DecoderValueInstancesFlatSpec
