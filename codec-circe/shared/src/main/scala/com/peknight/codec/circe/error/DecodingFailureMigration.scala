package com.peknight.codec.circe.error

import cats.Id
import com.peknight.codec.error.DecodingFailure
import com.peknight.error.std.WrongType
import com.peknight.generic.migration.Migration
import io.circe.ACursor
import io.circe.DecodingFailure.Reason.WrongTypeExpectation

trait DecodingFailureMigration extends Migration[Id, DecodingFailure[ACursor], io.circe.DecodingFailure]:
  def migrate(failure: DecodingFailure[ACursor]): io.circe.DecodingFailure =
    failure match
      case e: WrongType =>
        io.circe.DecodingFailure(WrongTypeExpectation(e.expectedType, e.value.focus.get), e.value.history)
      case _ => io.circe.DecodingFailure(failure.message, failure.value.history)
end DecodingFailureMigration
object DecodingFailureMigration extends DecodingFailureMigration