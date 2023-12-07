package com.peknight.codec.error

import com.peknight.codec.cursor.{Cursor, CursorOp}
import com.peknight.error.Error
import com.peknight.error.Error.Label

trait CursorDecodingFailure[S] extends DecodingFailure[Cursor[S]]:
  override def labelOption: Option[String] =
    Option(value.pathString.replaceFirst("^\\.", "")).filter(_.nonEmpty)
end CursorDecodingFailure
