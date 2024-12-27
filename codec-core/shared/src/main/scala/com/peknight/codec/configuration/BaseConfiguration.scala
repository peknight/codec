package com.peknight.codec.configuration

trait BaseConfiguration extends Configuration:
  type This <: BaseConfiguration
  def transformMemberNames: String => String
  def discriminator: Option[String]
  def extField: Option[String]
  def withTransformMemberNames(f: String => String): This
  def withDiscriminator(discriminator: String): This
  def withoutDiscriminator: This
  def withExtField(extField: String): This
  def withoutExtField: This
end BaseConfiguration
