package com.peknight.codec.config

import cats.data.NonEmptyList

trait Config:
  type This <: Config
  def transformConstructorNames: String => NonEmptyList[String]
  def withTransformConstructorName(f: String => String): This =
    withTransformConstructorNames(name => NonEmptyList.one(f(name)))
  def withTransformConstructorNames(f: String => NonEmptyList[String]): This
end Config