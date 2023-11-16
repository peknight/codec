package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.generic.migration.Migration
import com.peknight.codec.error.{DecodingFailure, WrongType}
import io.circe.DecodingFailure.Reason.WrongTypeExpectation
import io.circe.{ACursor, DecodingFailure as CirceDecodingFailure}

trait DecodingFailureMigration extends Migration[Id, DecodingFailure[ACursor], CirceDecodingFailure]:
  def migrate(failure: DecodingFailure[ACursor]): CirceDecodingFailure =
    failure match
      case e: WrongType[ACursor] =>
        CirceDecodingFailure(WrongTypeExpectation(e.expectedType, e.value.focus.get), e.value.history)
      case _ => CirceDecodingFailure(failure.message, failure.value.history)
end DecodingFailureMigration
object DecodingFailureMigration extends DecodingFailureMigration