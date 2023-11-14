package com.peknight.codec.ciris.instances

import com.peknight.codec.id.Encoder
import com.peknight.codec.syntax.decoder.decodeTo
import ciris.ConfigDecoder

import scala.concurrent.duration.{Duration, FiniteDuration}

trait ConfigDecoderLowPriorityInstances:
  private[this] type CirisDecoder[A] = ConfigDecoder[String, A]
  given stringDecoder[A](using Encoder[A, String]): CirisDecoder[A] =
    ConfigDecoder.identity[String].decodeTo[A]
  given bigDecimalDecoder[A](using Encoder[A, BigDecimal]): CirisDecoder[A] =
    ConfigDecoder.stringBigDecimalConfigDecoder.decodeTo[A]
  given bigIntDecoder[A](using Encoder[A, BigInt]): CirisDecoder[A] =
    ConfigDecoder.stringBigIntConfigDecoder.decodeTo[A]
  given booleanDecoder[A](using Encoder[A, Boolean]): CirisDecoder[A] =
    ConfigDecoder.stringBooleanConfigDecoder.decodeTo[A]
  given byteDecoder[A](using Encoder[A, Byte]): CirisDecoder[A] =
    ConfigDecoder.stringByteConfigDecoder.decodeTo[A]
  given charDecoder[A](using Encoder[A, Char]): CirisDecoder[A] =
    ConfigDecoder.stringCharConfigDecoder.decodeTo[A]
  given doubleDecoder[A](using Encoder[A, Double]): CirisDecoder[A] =
    ConfigDecoder.stringDoubleConfigDecoder.decodeTo[A]
  given durationDecoder[A](using Encoder[A, Duration]): CirisDecoder[A] =
    ConfigDecoder.stringDurationConfigDecoder.decodeTo[A]
  given finiteDurationDecoder[A](using Encoder[A, FiniteDuration]): CirisDecoder[A] =
    ConfigDecoder.stringFiniteDurationConfigDecoder.decodeTo[A]
  given floatDecoder[A](using Encoder[A, Float]): CirisDecoder[A] =
    ConfigDecoder.stringFloatConfigDecoder.decodeTo[A]
  given intDecoder[A](using Encoder[A, Int]): CirisDecoder[A] =
    ConfigDecoder.stringIntConfigDecoder.decodeTo[A]
  given longDecoder[A](using Encoder[A, Long]): CirisDecoder[A] =
    ConfigDecoder.stringLongConfigDecoder.decodeTo[A]
  given shortDecoder[A](using Encoder[A, Short]): CirisDecoder[A] =
    ConfigDecoder.stringShortConfigDecoder.decodeTo[A]

end ConfigDecoderLowPriorityInstances
