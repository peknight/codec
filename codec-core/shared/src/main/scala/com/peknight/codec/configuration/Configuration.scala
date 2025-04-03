package com.peknight.codec.configuration

import cats.data.NonEmptyList

trait Configuration:
  type This <: Configuration
  def transformConstructorNames: String => NonEmptyList[String]
  def withTransformConstructorNames(f: String => NonEmptyList[String]): This
end Configuration