package com.peknight.codec.circe.cursor

import com.peknight.codec.circe.instances.CursorOpInstances.given
import com.peknight.codec.cursor.CursorOp
import com.peknight.generic.migration.syntax.id.migration.migrateTo
import io.circe.{ACursor, Json, CursorOp as CirceCursorOp}

trait CursorType extends com.peknight.codec.cursor.CursorType[ACursor]:
  type Sum = Json
  def focus(t: ACursor): Option[Json] = t.focus
  def downField(t: ACursor, k: String): ACursor = t.downField(k)
  def pathString(t: ACursor): String = t.pathString
  def history(t: ACursor): List[CursorOp] = t.history.map(_.migrateTo[CursorOp])
end CursorType
object CursorType extends CursorType
