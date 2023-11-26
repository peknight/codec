package com.peknight.codec.cursor

trait SuccessCursor[S] extends Cursor[S]:
  def succeeded: Boolean = true
end SuccessCursor
