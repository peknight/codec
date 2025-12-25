package com.peknight.codec.circe

import cats.syntax.either.*
import com.peknight.codec.circe.OuterSum.*
import com.peknight.circe.instances.json.given
import com.peknight.codec.circe.instances.all.given
import com.peknight.codec.config.CodecConfig
import com.peknight.codec.instances.generic.decoder.derivation.given
import com.peknight.codec.instances.generic.encoder.derivation.given
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, Encoder}
import org.scalatest.flatspec.AnyFlatSpec

class SumCirceInstancesFlatSpec extends AnyFlatSpec:
  "Sum Codec" should "pass" in {
    val a: OuterSum = A
    val b: OuterSum = B
    val c: OuterSum = C
    val d: OuterSum = D("dddd")
    val e: OuterSum = E
    val f: OuterSum = F("ffffff")
    given CodecConfig = CodecConfig(discriminator = Some("type"))
    given CanEqual[Result[OuterSum], Result[OuterSum]] = CanEqual.derived
    val decoder: Decoder[OuterSum] = Decoder[OuterSum]
    val encoder: Encoder[OuterSum] = Encoder[OuterSum]
    assert(decoder.decodeJson(encoder(a)) == a.asRight[DecodingFailure])
    assert(decoder.decodeJson(encoder(b)) == b.asRight[DecodingFailure])
    assert(decoder.decodeJson(encoder(c)) == c.asRight[DecodingFailure])
    assert(decoder.decodeJson(encoder(d)) == d.asRight[DecodingFailure])
    assert(decoder.decodeJson(encoder(e)) == e.asRight[DecodingFailure])
    assert(decoder.decodeJson(encoder(f)) == f.asRight[DecodingFailure])
  }
end SumCirceInstancesFlatSpec
