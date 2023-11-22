package com.peknight.codec.circe

import com.peknight.codec.circe.OuterSum.*
import com.peknight.codec.circe.derivation.CodecDerivation
import com.peknight.codec.circe.derivation.all.given
// import com.peknight.codec.derivation.all.given
// import com.peknight.codec.circe.derivation.CursorTypeInstances.given
// import com.peknight.codec.circe.derivation.ObjectTypeInstances.given
// import com.peknight.codec.circe.derivation.DecodingFailureMigrationInstances.given
// import com.peknight.codec.circe.instances.EncoderInstances.given
// import com.peknight.codec.circe.instances.DecoderInstances.given
import com.peknight.codec.configuration.CodecConfiguration
import io.circe.Codec
import org.scalatest.flatspec.AnyFlatSpec

class SumCirceInstancesFlatSpec extends AnyFlatSpec:
  "Sum Encoder" should "succeed" in {
    val a: OuterSum = A
    val b: OuterSum = B
    val c: OuterSum = C
    val d: OuterSum = D("dddd")
    given CodecConfiguration = CodecConfiguration(discriminator = Some("rua"))
    given codec: Codec[OuterSum] = CodecDerivation.derived[OuterSum]
    val aJson = codec.apply(a)
    val bJson = codec.apply(b)
    val cJson = codec.apply(c)
    val dJson = codec.apply(d)
    val aResult = codec.decodeJson(aJson)
    val bResult = codec.decodeJson(bJson)
    val cResult = codec.decodeJson(cJson)
    val dResult = codec.decodeJson(dJson)
    println(aJson)
    println(aResult)
    println(bJson)
    println(bResult)
    println(cJson)
    println(cResult)
    println(dJson)
    println(dResult)
    assert(true)
  }
end SumCirceInstancesFlatSpec
