package com.peknight.codec.circe.error

import com.peknight.codec.circe.instances.CursorOpInstances.given
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.error.DecodingFailure.Common
import com.peknight.error.Error
import com.peknight.error.std.WrongType
import com.peknight.generic.migration.id.Migration
import com.peknight.generic.migration.syntax.id.migration.migrateTo
import io.circe.DecodingFailure.Reason.WrongTypeExpectation
import io.circe.{CursorOp, Json}

trait DecodingFailureMigration extends Migration[DecodingFailure, io.circe.DecodingFailure]:
  def migrate(failure: DecodingFailure): io.circe.DecodingFailure =
    failure match
      case Common(e: WrongType, _, _, Some(value: Json), _, _, Some(history)) =>
        io.circe.DecodingFailure(WrongTypeExpectation(e.expectedType, value), history.map(_.migrateTo[CursorOp]))
      case Common(e: Error, _, _, _, _, _, Some(history)) =>
        io.circe.DecodingFailure(e.message, history.map(_.migrateTo[CursorOp]))
      case Common(e, _, _, _, _, _, Some(history)) =>
        io.circe.DecodingFailure(Error.pureMessage(e), history.map(_.migrateTo[CursorOp]))
      case _ => io.circe.DecodingFailure(failure.message, List.empty[CursorOp])
end DecodingFailureMigration
object DecodingFailureMigration extends DecodingFailureMigration