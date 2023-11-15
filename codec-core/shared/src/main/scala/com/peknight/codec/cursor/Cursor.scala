package com.peknight.codec.cursor

trait Cursor[S]:
  def value: Option[S]

