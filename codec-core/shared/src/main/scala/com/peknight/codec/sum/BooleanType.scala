package com.peknight.codec.sum

trait BooleanType[S]:
  def to(b: Boolean): S
  def asBoolean(s: S): Option[Boolean]
  def isBoolean(s: S): Boolean = asBoolean(s).isDefined
end BooleanType
object BooleanType:
  def apply[S](using booleanType: BooleanType[S]): BooleanType[S] = booleanType
  def apply[S](f: Boolean => S, g: S => Option[Boolean]): BooleanType[S] =
    new BooleanType[S]:
      def to(s: Boolean): S = f(s)
      def asBoolean(s: S): Option[Boolean] = g(s)
  end apply
end BooleanType
