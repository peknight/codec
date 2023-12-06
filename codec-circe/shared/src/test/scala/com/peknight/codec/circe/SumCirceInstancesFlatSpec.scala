package com.peknight.codec.circe

import cats.syntax.either.*
import com.peknight.codec.circe.OuterSum.*
import com.peknight.codec.circe.derivation.CodecDerivation
import com.peknight.codec.circe.derivation.all.given
import com.peknight.codec.configuration.CodecConfiguration
import io.circe.Decoder.Result
import io.circe.{Codec, DecodingFailure}
import org.scalatest.flatspec.AnyFlatSpec

class SumCirceInstancesFlatSpec extends AnyFlatSpec:
  "Sum Encoder" should "succeed" in {
    val a: OuterSum = A
    val b: OuterSum = B
    val c: OuterSum = C
    val d: OuterSum = D("dddd")
    val e: OuterSum = E
    val f: OuterSum = F("ffffff")
    given CodecConfiguration = CodecConfiguration(discriminator = Some("type"))
    given codec: Codec[OuterSum] = CodecDerivation.derived[OuterSum]
    given CanEqual[Result[OuterSum], Result[OuterSum]] = CanEqual.derived
    assert(codec.decodeJson(codec(a)) == a.asRight[DecodingFailure])
    assert(codec.decodeJson(codec(b)) == b.asRight[DecodingFailure])
    assert(codec.decodeJson(codec(c)) == c.asRight[DecodingFailure])
    assert(codec.decodeJson(codec(d)) == d.asRight[DecodingFailure])
    assert(codec.decodeJson(codec(e)) == e.asRight[DecodingFailure])
    assert(codec.decodeJson(codec(f)) == f.asRight[DecodingFailure])
  }
end SumCirceInstancesFlatSpec
