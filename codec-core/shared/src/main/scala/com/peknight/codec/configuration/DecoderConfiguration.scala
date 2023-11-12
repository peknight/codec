package com.peknight.codec.configuration

trait DecoderConfiguration extends Configuration:
  type This <: DecoderConfiguration
  def useDefaults: Boolean
  def strictDecoding: Boolean
  def withDefaults: This
  def withoutDefaults: This
  def withStrictDecoding: This
  def withoutStrictDecoding: This
end DecoderConfiguration

