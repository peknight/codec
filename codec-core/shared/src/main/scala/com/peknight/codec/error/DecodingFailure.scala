package com.peknight.codec.error

import com.peknight.error.Error.Value
import com.peknight.codec.cursor.Cursor

trait DecodingFailure[A] extends com.peknight.error.codec.DecodingFailure with Value[A]:
  override def labelOption: Option[String] =
    value match
      case cursor: Cursor[_] => Option(cursor.pathString.replaceFirst("^\\.", "")).filter(_.nonEmpty)
      case _ => None
end DecodingFailure
