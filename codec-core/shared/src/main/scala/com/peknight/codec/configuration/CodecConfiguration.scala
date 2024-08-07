package com.peknight.codec.configuration

trait CodecConfiguration extends EncoderConfiguration with DecoderConfiguration:
  type This <: CodecConfiguration
end CodecConfiguration

object CodecConfiguration:
  private case class Configuration(
    transformMemberNames: String => String,
    transformConstructorNames: String => String,
    discriminator: Option[String],
    extendedField: Option[String],
    useDefaults: Boolean,
    strictDecoding: Boolean
  ) extends CodecConfiguration:
    type This = Configuration
    def withTransformMemberNames(f: String => String): Configuration = copy(transformMemberNames = f)
    def withTransformConstructorNames(f: String => String): Configuration = copy(transformConstructorNames = f)
    def withDiscriminator(discriminator: String): Configuration = copy(discriminator = Some(discriminator))
    def withoutDiscriminator: Configuration = copy(discriminator = None)
    def withExtendedField(extendedField: String): Configuration = copy(extendedField = Some(extendedField))
    def withoutExtendedField: Configuration = copy(extendedField = None)
    def withDefaults: Configuration = copy(useDefaults = true)
    def withoutDefaults: Configuration = copy(useDefaults = false)
    def withStrictDecoding: Configuration = copy(strictDecoding = true)
    def withoutStrictDecoding: Configuration = copy(strictDecoding = false)
  end Configuration

  def apply(
    transformMemberNames: String => String = Predef.identity,
    transformConstructorNames: String => String = Predef.identity,
    discriminator: Option[String] = None,
    extendedField: Option[String] = None,
    useDefaults: Boolean = false,
    strictDecoding: Boolean = false
  ): CodecConfiguration =
    Configuration(transformMemberNames, transformConstructorNames, discriminator, extendedField, useDefaults, strictDecoding)

  val default: com.peknight.codec.configuration.CodecConfiguration = apply()

end CodecConfiguration
