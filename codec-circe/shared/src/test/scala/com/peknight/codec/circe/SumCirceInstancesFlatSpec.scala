package com.peknight.codec.circe

import cats.syntax.either.*
import com.peknight.codec.circe.OuterSum.*
import com.peknight.codec.circe.instances.all.given
import com.peknight.codec.configuration.CodecConfiguration
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, Encoder}
import org.scalatest.flatspec.AnyFlatSpec

class SumCirceInstancesFlatSpec extends AnyFlatSpec:
  "Sum Codec" should "succeed" in {
    val a: OuterSum = A
    val b: OuterSum = B
    val c: OuterSum = C
    val d: OuterSum = D("dddd")
    val e: OuterSum = E
    val f: OuterSum = F("ffffff")
    given CodecConfiguration = CodecConfiguration(discriminator = Some("type"))
    given CanEqual[Result[OuterSum], Result[OuterSum]] = CanEqual.derived
    assert(Decoder[OuterSum].decodeJson(Encoder[OuterSum].apply(a)) == a.asRight[DecodingFailure])
    assert(Decoder[OuterSum].decodeJson(Encoder[OuterSum].apply(b)) == b.asRight[DecodingFailure])
    assert(Decoder[OuterSum].decodeJson(Encoder[OuterSum].apply(c)) == c.asRight[DecodingFailure])
    assert(Decoder[OuterSum].decodeJson(Encoder[OuterSum].apply(d)) == d.asRight[DecodingFailure])
    assert(Decoder[OuterSum].decodeJson(Encoder[OuterSum].apply(e)) == e.asRight[DecodingFailure])
    assert(Decoder[OuterSum].decodeJson(Encoder[OuterSum].apply(f)) == f.asRight[DecodingFailure])
  }
end SumCirceInstancesFlatSpec
