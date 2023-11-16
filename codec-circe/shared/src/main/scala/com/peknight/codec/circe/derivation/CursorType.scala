package com.peknight.codec.circe.derivation

import io.circe.{ACursor, Json}

trait CursorType extends com.peknight.codec.derivation.CursorType[ACursor]:
  type Sum = Json
  def focus(t: ACursor): Option[Json] = t.focus
  def downField(t: ACursor, k: String): ACursor = t.downField(k)
end CursorType
object CursorType extends CursorType
