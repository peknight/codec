package com.peknight.codec.configuration

trait Configuration:
  type This <: Configuration
  def transformMemberNames: String => String
  def transformConstructorNames: String => String
  def discriminator: Option[String]
  def withTransformMemberNames(f: String => String): This
  def withTransformConstructorNames(f: String => String): This
  def withDiscriminator(discriminator: String): This
  def withoutDiscriminator: This
end Configuration