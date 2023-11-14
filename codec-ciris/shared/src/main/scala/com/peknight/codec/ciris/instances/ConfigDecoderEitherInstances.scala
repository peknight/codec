package com.peknight.codec.ciris.instances

import ciris.{ConfigDecoder, ConfigError}
import com.peknight.codec.id.Decoder

import scala.concurrent.duration.{Duration, FiniteDuration}

trait ConfigDecoderEitherInstances:
  extension [T] (configDecoder: ConfigDecoder[String, T])
    private[this] def to[A](using decoder: Decoder[T, ConfigError, A]): ConfigDecoder[String, A] =
      configDecoder.mapEither((_, t) => decoder.decode(t))
  end extension
  given stringEitherDecoder[A](using Decoder[String, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.identity[String].to[A]
  given bigDecimalEitherDecoder[A](using Decoder[BigDecimal, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringBigDecimalConfigDecoder.to[A]
  given bigIntEitherDecoder[A](using Decoder[BigInt, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringBigIntConfigDecoder.to[A]
  given booleanEitherDecoder[A](using Decoder[Boolean, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringBooleanConfigDecoder.to[A]
  given byteEitherDecoder[A](using Decoder[Byte, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringByteConfigDecoder.to[A]
  given charEitherDecoder[A](using Decoder[Char, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringCharConfigDecoder.to[A]
  given doubleEitherDecoder[A](using Decoder[Double, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringDoubleConfigDecoder.to[A]
  given durationEitherDecoder[A](using Decoder[Duration, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringDurationConfigDecoder.to[A]
  given finiteDurationEitherDecoder[A](using Decoder[FiniteDuration, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringFiniteDurationConfigDecoder.to[A]
  given floatEitherDecoder[A](using Decoder[Float, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringFloatConfigDecoder.to[A]
  given intEitherDecoder[A](using Decoder[Int, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringIntConfigDecoder.to[A]
  given longEitherDecoder[A](using Decoder[Long, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringLongConfigDecoder.to[A]
  given shortEitherDecoder[A](using Decoder[Short, ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringShortConfigDecoder.to[A]

end ConfigDecoderEitherInstances
