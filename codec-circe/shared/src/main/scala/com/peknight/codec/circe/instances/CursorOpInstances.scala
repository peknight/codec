package com.peknight.codec.circe.instances

import cats.Id
import com.peknight.codec.cursor.CursorOp
import com.peknight.generic.migration.id.Isomorphism
import io.circe.CursorOp as CirceCursorOp

trait CursorOpInstances:
  given Isomorphism[CursorOp, CirceCursorOp] with
    given CanEqual[CirceCursorOp, CirceCursorOp] = CanEqual.derived
    def to(a: CursorOp): Id[CirceCursorOp] =
      a match
        case CursorOp.MoveLeft => CirceCursorOp.MoveLeft
        case CursorOp.MoveRight => CirceCursorOp.MoveRight
        case CursorOp.MoveUp => CirceCursorOp.MoveUp
        case CursorOp.Field(k) => CirceCursorOp.Field(k)
        case CursorOp.DownField(k) => CirceCursorOp.DownField(k)
        case CursorOp.DownArray => CirceCursorOp.DownArray
        case CursorOp.DownN(n) => CirceCursorOp.DownN(n)
        case CursorOp.DeleteGoParent => CirceCursorOp.DeleteGoParent

    def from(b: CirceCursorOp): Id[CursorOp] =
      b match
        case CirceCursorOp.MoveLeft => CursorOp.MoveLeft
        case CirceCursorOp.MoveRight => CursorOp.MoveRight
        case CirceCursorOp.MoveUp => CursorOp.MoveUp
        case CirceCursorOp.Field(k) => CursorOp.Field(k)
        case CirceCursorOp.DownField(k) => CursorOp.DownField(k)
        case CirceCursorOp.DownArray => CursorOp.DownArray
        case CirceCursorOp.DownN(n) => CursorOp.DownN(n)
        case CirceCursorOp.DeleteGoParent => CursorOp.DeleteGoParent
  end given
end CursorOpInstances
object CursorOpInstances extends CursorOpInstances
