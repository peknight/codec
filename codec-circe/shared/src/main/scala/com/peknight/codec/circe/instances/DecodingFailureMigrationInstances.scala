package com.peknight.codec.circe.instances

import com.peknight.codec.error.{DecodingFailure, WrongType}
import com.peknight.generic.migration.id.Migration
import io.circe.DecodingFailure.Reason.WrongTypeExpectation
import io.circe.{ACursor, DecodingFailure as CirceDecodingFailure}

trait DecodingFailureMigrationInstances:
  given decodingFailureMigration: Migration[DecodingFailure[ACursor], CirceDecodingFailure] with
    def migrate(failure: DecodingFailure[ACursor]): CirceDecodingFailure =
      failure match
        case e: WrongType[ACursor] =>
          CirceDecodingFailure(WrongTypeExpectation(e.expectedType, e.value.focus.get), e.value.history)
        case _ => CirceDecodingFailure(failure.message, failure.value.history)
  end decodingFailureMigration
end DecodingFailureMigrationInstances
object DecodingFailureMigrationInstances extends DecodingFailureMigrationInstances
