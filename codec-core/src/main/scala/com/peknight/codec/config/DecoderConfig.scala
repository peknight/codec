package com.peknight.codec.config

trait DecoderConfig extends BaseConfig:
  type This <: DecoderConfig
  def useDefaults: Boolean
  def strictDecoding: Boolean
  def withDefaults: This
  def withoutDefaults: This
  def withStrictDecoding: This
  def withoutStrictDecoding: This
end DecoderConfig

