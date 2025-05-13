package com.peknight.codec.config

import cats.data.NonEmptyList

trait CodecConfig extends EncoderConfig with DecoderConfig:
  type This <: CodecConfig
end CodecConfig

object CodecConfig:
  private case class Config(
    transformMemberNames: String => NonEmptyList[String],
    transformConstructorNames: String => NonEmptyList[String],
    discriminator: Option[String],
    sumTypeOnNone: Option[String],
    extField: Option[String],
    flattenFields: List[String],
    useDefaults: Boolean,
    strictDecoding: Boolean
  ) extends CodecConfig:
    type This = Config
    def withTransformMemberNames(f: String => NonEmptyList[String]): Config = copy(transformMemberNames = f)
    def withTransformConstructorNames(f: String => NonEmptyList[String]): Config = copy(transformConstructorNames = f)
    def withDiscriminator(discriminator: String): Config = copy(discriminator = Some(discriminator))
    def withoutDiscriminator: Config = copy(discriminator = None)
    def withSumTypeOnNone(sumTypeOnNone: String): Config = copy(sumTypeOnNone = Some(sumTypeOnNone))
    def withoutSumTypeOnNone: Config = copy(sumTypeOnNone = None)
    def withExtField(extField: String): Config = copy(extField = Some(extField))
    def withoutExtField: Config = copy(extField = None)
    def withFlattenFields(flattenFields: List[String]): Config = copy(flattenFields = flattenFields)
    def withoutFlattenFields: Config = copy(flattenFields = Nil)
    def withDefaults: Config = copy(useDefaults = true)
    def withoutDefaults: Config = copy(useDefaults = false)
    def withStrictDecoding: Config = copy(strictDecoding = true)
    def withoutStrictDecoding: Config = copy(strictDecoding = false)
  end Config

  def apply(
    transformMemberNames: String => NonEmptyList[String] = NonEmptyList.one,
    transformConstructorNames: String => NonEmptyList[String] = NonEmptyList.one,
    discriminator: Option[String] = None,
    sumTypeOnNone: Option[String] = None,
    extField: Option[String] = None,
    flattenFields: List[String] = Nil,
    useDefaults: Boolean = true,
    strictDecoding: Boolean = false
  ): CodecConfig =
    Config(transformMemberNames, transformConstructorNames, discriminator, sumTypeOnNone, extField, flattenFields,
      useDefaults, strictDecoding)

  val default: com.peknight.codec.config.CodecConfig = apply()

end CodecConfig
