package com.peknight.codec.configuration

import cats.data.NonEmptyList

trait CodecConfiguration extends EncoderConfiguration with DecoderConfiguration:
  type This <: CodecConfiguration
end CodecConfiguration

object CodecConfiguration:
  private case class Configuration(
    transformMemberNames: String => NonEmptyList[String],
    transformConstructorNames: String => NonEmptyList[String],
    discriminator: Option[String],
    sumTypeOnNone: Option[String],
    extField: Option[String],
    useDefaults: Boolean,
    strictDecoding: Boolean
  ) extends CodecConfiguration:
    type This = Configuration
    def withTransformMemberNames(f: String => NonEmptyList[String]): Configuration = copy(transformMemberNames = f)
    def withTransformConstructorNames(f: String => NonEmptyList[String]): Configuration = copy(transformConstructorNames = f)
    def withDiscriminator(discriminator: String): Configuration = copy(discriminator = Some(discriminator))
    def withoutDiscriminator: Configuration = copy(discriminator = None)
    def withSumTypeOnNone(sumTypeOnNone: String): Configuration = copy(sumTypeOnNone = Some(sumTypeOnNone))
    def withoutSumTypeOnNone: Configuration = copy(sumTypeOnNone = None)
    def withExtField(extField: String): Configuration = copy(extField = Some(extField))
    def withoutExtField: Configuration = copy(extField = None)
    def withDefaults: Configuration = copy(useDefaults = true)
    def withoutDefaults: Configuration = copy(useDefaults = false)
    def withStrictDecoding: Configuration = copy(strictDecoding = true)
    def withoutStrictDecoding: Configuration = copy(strictDecoding = false)
  end Configuration

  def apply(
    transformMemberNames: String => NonEmptyList[String] = NonEmptyList.one,
    transformConstructorNames: String => NonEmptyList[String] = NonEmptyList.one,
    discriminator: Option[String] = None,
    sumTypeOnNone: Option[String] = None,
    extField: Option[String] = None,
    useDefaults: Boolean = true,
    strictDecoding: Boolean = false
  ): CodecConfiguration =
    Configuration(transformMemberNames, transformConstructorNames, discriminator, sumTypeOnNone, extField, useDefaults,
      strictDecoding)

  val default: com.peknight.codec.configuration.CodecConfiguration = apply()

end CodecConfiguration
