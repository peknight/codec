package io.circe.cursor

import io.circe.{CursorOp, HCursor, Json, JsonObject}

object CursorOps:
  def topCursor(value: Json, lastCursor: HCursor, lastOp: CursorOp): HCursor = TopCursor(value)(lastCursor, lastOp)
  def objectCursor(obj: JsonObject, keyValue: String, parent: HCursor, changed: Boolean, lastCursor: HCursor,
                   lastOp: CursorOp): HCursor =
    ObjectCursor(obj, keyValue, parent, changed)(lastCursor, lastOp)
  def arrayCursor(values: Vector[Json], indexValue: Int, parent: HCursor, changed: Boolean, lastCursor: HCursor,
                  lastOp: CursorOp): HCursor =
    ArrayCursor(values, indexValue, parent, changed)(lastCursor, lastOp)
end CursorOps
