package com.peknight.codec.configuration

import cats.data.NonEmptyList

trait Configuration:
  type This <: Configuration
  def transformConstructorNames: String => NonEmptyList[String]
  def withTransformConstructorName(f: String => String): This =
    withTransformConstructorNames(name => NonEmptyList.one(f(name)))
  def withTransformConstructorNames(f: String => NonEmptyList[String]): This
end Configuration