package com.peknight.codec.ciris.instances

import ciris.{ConfigDecoder, ConfigError, ConfigKey}
import com.peknight.codec.id.Decoder

import scala.concurrent.duration.{Duration, FiniteDuration}

trait ConfigDecoderInstances extends ConfigDecoderEitherInstances with ConfigDecoderLowPriorityInstances:
  extension[T] (configDecoder: ConfigDecoder[String, T])
    private[this] def to[A](using decoder: Decoder[(Option[ConfigKey], T), ConfigError, A]): ConfigDecoder[String, A] =
      configDecoder.mapEither((key, t) => decoder.decode((key, t)))
  end extension
  given stringKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], String), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.identity[String].to[A]
  given bigDecimalKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], BigDecimal), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringBigDecimalConfigDecoder.to[A]
  given bigIntKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], BigInt), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringBigIntConfigDecoder.to[A]
  given booleanKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], Boolean), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringBooleanConfigDecoder.to[A]
  given byteKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], Byte), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringByteConfigDecoder.to[A]
  given charKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], Char), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringCharConfigDecoder.to[A]
  given doubleKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], Double), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringDoubleConfigDecoder.to[A]
  given durationKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], Duration), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringDurationConfigDecoder.to[A]
  given finiteDurationKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], FiniteDuration), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringFiniteDurationConfigDecoder.to[A]
  given floatKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], Float), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringFloatConfigDecoder.to[A]
  given intKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], Int), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringIntConfigDecoder.to[A]
  given longKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], Long), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringLongConfigDecoder.to[A]
  given shortKeyEitherDecoder[A](using Decoder[(Option[ConfigKey], Short), ConfigError, A]): ConfigDecoder[String, A] =
    ConfigDecoder.stringShortConfigDecoder.to[A]
end ConfigDecoderInstances
object ConfigDecoderInstances extends ConfigDecoderInstances
