package com.peknight.codec.cursor

trait FailedCursor[S] extends Cursor[S]:
  def succeeded: Boolean = false
end FailedCursor
