package com.peknight.codec.circe.parser

import cats.{Applicative, Id, Monad}
import com.peknight.codec.Codec
import com.peknight.codec.circe.iso.codec
import com.peknight.codec.circe.sum.jsonType.given
import com.peknight.codec.configuration.given
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.instances.time.instant.{codecInstantOfEpochMilliNS, codecInstantOfEpochSecondNS}
import com.peknight.codec.sum.{NullType, NumberType, ObjectType, StringType}
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec

import java.time.Instant

class InstantDecoderFlatSpec extends AnyFlatSpec:

  case class Box(time: Instant)

  "Instant Decoder" should "succeed with epoch second" in {
    given codecInstant[F[_] : Applicative, S: NumberType : StringType]: Codec[F, S, Cursor[S], Instant] =
      codecInstantOfEpochSecondNS[F, S]
    given codecBox[F[_]: Monad, S: ObjectType: NullType: NumberType: StringType]: Codec[F, S, Cursor[S], Box] =
      Codec.derived[F, S, Box]
    assert(decode[Id, Box]("""{"time":1234567890.012345}""")
      .exists(box => box.time.compareTo(Instant.ofEpochSecond(1234567890L, 12345000L)) == 0)
    )
  }

  "Instant Decoder" should "succeed with epoch milli" in {
    given codecInstant[F[_] : Applicative, S: NumberType : StringType]: Codec[F, S, Cursor[S], Instant] =
      codecInstantOfEpochMilliNS[F, S]
    given codecBox[F[_]: Monad, S: ObjectType: NullType: NumberType: StringType]: Codec[F, S, Cursor[S], Box] =
      Codec.derived[F, S, Box]
    given jsonCodecBox[F[_]: Monad]: Codec[F, Json, Cursor[Json], Box] = codecBox[F, Json]
    given circeCodecBox: io.circe.Codec[Box] = codec[Box]
    assert(decode[Id, Box]("""{"time":1234567890012.345}""")
      .exists(box => box.time.compareTo(Instant.ofEpochSecond(1234567890L, 12345000L)) == 0)
    )
  }
end InstantDecoderFlatSpec
