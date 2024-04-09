package io.circe

import com.peknight.codec.circe.iso.biggerDecimalIsomorphism
import com.peknight.codec.number.Number
import io.circe.numbers.BiggerDecimal

object JsonNumberOps:
  def fromBiggerDecimal(value: BiggerDecimal, input: String): JsonNumber = JsonBiggerDecimal(value, input)
  def fromBigDecimal(value: BigDecimal): JsonNumber = JsonBigDecimal(value.underlying())
  def fromLong(value: Long): JsonNumber = JsonLong(value)
  def fromDouble(value: Double): JsonNumber = JsonDouble(value)
  def fromFloat(value: Float): JsonNumber = JsonFloat(value)

  def migrate(jsonNumber: JsonNumber): Number =
    jsonNumber match
      case JsonDecimal(input) => Number.fromDecimalStringUnsafe(input)
      case JsonBiggerDecimal(value, input) => Number.fromBiggerDecimal(biggerDecimalIsomorphism.from(value), input)
      case JsonBigDecimal(value) => Number.fromBigDecimal(BigDecimal(value))
      case JsonLong(value) => Number.fromLong(value)
      case JsonDouble(value) => Number.fromDouble(value)
      case JsonFloat(value) => Number.fromFloat(value)
end JsonNumberOps
