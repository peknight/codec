package com.peknight.codec.configuration

import cats.data.NonEmptyList

trait BaseConfiguration extends Configuration:
  type This <: BaseConfiguration
  def transformMemberNames: String => NonEmptyList[String]
  def discriminator: Option[String]
  def sumTypeOnNone: Option[String]
  def extField: Option[String]
  def withTransformMemberNames(f: String => NonEmptyList[String]): This
  def withDiscriminator(discriminator: String): This
  def withoutDiscriminator: This
  def withSumTypeOnNone(sumTypeOnNone: String): This
  def withoutSumTypeOnNone: This
  def withExtField(extField: String): This
  def withoutExtField: This
end BaseConfiguration
