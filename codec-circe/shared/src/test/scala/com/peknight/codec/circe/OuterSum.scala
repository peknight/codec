package com.peknight.codec.circe

sealed trait OuterSum
object OuterSum:
  sealed trait EnumSum extends OuterSum
  case object A extends EnumSum
  case object B extends EnumSum

  sealed trait InnerSum extends OuterSum
  case object C extends InnerSum
  case class D(d: String) extends InnerSum

  case object E extends OuterSum
  case class F(f: String) extends OuterSum
end OuterSum
