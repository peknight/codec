package com.peknight.codec.config

import cats.data.NonEmptyList

trait BaseConfig extends Config:
  type This <: BaseConfig
  def transformMemberNames: String => NonEmptyList[String]
  def discriminator: Option[String]
  def sumTypeOnNone: Option[String]
  def extField: Option[String]
  def flattenFields: List[String]
  def withTransformMemberName(f: String => String): This =
    withTransformMemberNames(name => NonEmptyList.one(f(name)))
  def withTransformMemberNames(f: String => NonEmptyList[String]): This
  def withDiscriminator(discriminator: String): This
  def withoutDiscriminator: This
  def withSumTypeOnNone(sumTypeOnNone: String): This
  def withoutSumTypeOnNone: This
  def withExtField(extField: String): This
  def withoutExtField: This
  def withFlattenField(flattenField: String): This = withFlattenFields(List(flattenField))
  def withFlattenFields(flattenFields: String*): This = withFlattenFields(flattenFields.toList)
  def withFlattenFields(flattenFields: List[String]): This
  def withoutFlattenFields: This
end BaseConfig
