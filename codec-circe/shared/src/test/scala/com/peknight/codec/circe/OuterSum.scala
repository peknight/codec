package com.peknight.codec.circe

sealed trait OuterSum
object OuterSum:
  sealed trait InnerSum extends OuterSum
  case object A extends InnerSum
  case object B extends InnerSum

  case object C extends OuterSum
  case class D(d: String) extends OuterSum
end OuterSum
