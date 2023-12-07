package com.peknight.codec.configuration

trait Configuration:
  type This <: Configuration
  def transformConstructorNames: String => String
  def withTransformConstructorNames(f: String => String): This
end Configuration