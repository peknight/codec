package com.peknight.codec.instances

import cats.Id
import com.peknight.codec.{Decoder, Encoder}
import org.scalatest.flatspec.AnyFlatSpec

import java.time.Instant
import scala.concurrent.duration.*

class DecoderValueInstancesFlatSpec extends AnyFlatSpec:
  "Instant Decoder" should "pass" in {
    assert(Decoder[Id, String, Instant].decode("2011-12-03T10:15:30Z").isRight)
    assert(Decoder[Id, String, Instant].decode("2021-06-10T15:17:27.987721Z").isRight)
    assert(Encoder[Id, String, FiniteDuration].encode(15.seconds) === "15 seconds")
    assert(Decoder[Id, String, FiniteDuration].decode("13.3s").contains(13.3.seconds))
  }
end DecoderValueInstancesFlatSpec
