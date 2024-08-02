package com.peknight.codec.configuration

trait BaseConfiguration extends Configuration:
  type This <: BaseConfiguration
  def transformMemberNames: String => String
  def discriminator: Option[String]
  def extendedField: Option[String]
  def withTransformMemberNames(f: String => String): This
  def withDiscriminator(discriminator: String): This
  def withoutDiscriminator: This
  def withExtendedField(extendedField: String): This
  def withoutExtendedField: This
end BaseConfiguration
