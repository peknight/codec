package io.circe

import cats.Id
import com.peknight.codec.circe.iso
import com.peknight.codec.circe.iso.biggerDecimalIsomorphism
import com.peknight.codec.number.Number

object JsonNumberCodecOps:
  def migrate(jsonNumber: JsonNumber): Number =
    jsonNumber match
      case JsonDecimal(input) => Number.fromStringUnsafe(input)
      case JsonBiggerDecimal(value, input) => Number.fromBiggerDecimal(biggerDecimalIsomorphism[Id].from(value))
      case JsonBigDecimal(value) => Number.fromBigDecimal(BigDecimal(value))
      case JsonLong(value) => Number.fromLong(value)
      case JsonDouble(value) => Number.fromDouble(value)
      case JsonFloat(value) => Number.fromFloat(value)
end JsonNumberCodecOps
