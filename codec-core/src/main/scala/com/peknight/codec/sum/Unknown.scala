package com.peknight.codec.sum

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.option.*
import com.peknight.codec.config.{CodecConfig, Config}
import com.peknight.codec.derivation.{EnumCodecDerivation, EnumDecoderDerivation, EnumEncoderDerivation}
import com.peknight.codec.{Codec, Decoder, Encoder}
import com.peknight.generic.Generic

trait Unknown
object Unknown:
  val sumType = "Unknown"
  def constructorNameMap[A](f: A => String = (a: A) => a.toString, unknownType: String = sumType,
                            unknownMappingNames: NonEmptyList[String] = NonEmptyList.of(sumType, ""))
                           (using generic: Generic[A]): Map[String, NonEmptyList[String]] =
    generic.labels.zip(generic.singletons).toList
      .asInstanceOf[List[(String, Option[A])]]
      .collect {
        case (key, Some(value)) => (key, NonEmptyList.one(f(value)))
      }
      .toMap + (unknownType -> unknownMappingNames)

  def config[A: Generic](f: A => String = (a: A) => a.toString, unknownType: String = sumType,
                         unknownMappingNames: NonEmptyList[String] = NonEmptyList.of(sumType, "")): Config =
    val constructorNames = constructorNameMap(f, unknownType, unknownMappingNames)
    CodecConfig.default.withTransformConstructorNames(constructorName =>
      constructorNames.getOrElse(constructorName, NonEmptyList.one(constructorName))
    )

  def derivedStringEncodeEnum[F[_]: Applicative, A: Generic.Sum](f: PartialFunction[A, String] = PartialFunction.empty)
                                                                (nameF: A => String = (a: A) => a.toString,
                                                                 unknownType: String = sumType,
                                                                 unknownMappingNames: NonEmptyList[String] = NonEmptyList.of(sumType, ""))
  : Encoder[F, String, A] =
    given Config = config[A](nameF, unknownType, unknownMappingNames)
    EnumEncoderDerivation.partialDerivedStringEncodeEnum[F, A](f)

  def derivedStringDecodeEnum[F[_]: Applicative, A: Generic.Sum](f: String => A)
                                                                (nameF: A => String = (a: A) => a.toString,
                                                                 unknownType: String = sumType,
                                                                 unknownMappingNames: NonEmptyList[String] = NonEmptyList.of(sumType, ""))
  : Decoder[F, String, A] =
    given Config = config[A](nameF, unknownType, unknownMappingNames)
    EnumDecoderDerivation.partialDerivedStringDecodeEnum[F, A](a => f(a).some)

  def derivedStringCodecEnum[F[_]: Applicative, A: Generic.Sum](f: PartialFunction[A, String] = PartialFunction.empty)
                                                               (g: String => A)
                                                               (nameF: A => String = (a: A) => a.toString,
                                                                unknownType: String = sumType,
                                                                unknownMappingNames: NonEmptyList[String] = NonEmptyList.of(sumType, ""))
  : Codec[F, String, String, A] =
    given Config = config[A](nameF, unknownType, unknownMappingNames)
    EnumCodecDerivation.partialDerivedStringCodecEnum[F, A](f)(a => g(a).some)
end Unknown
