package com.peknight.codec.sum

trait StringType[S]:
  def to(s: String): S
  def asString(s: S): Option[String]
  def isString(s: S): Boolean = asString(s).isDefined
end StringType
object StringType:
  def apply[S](using stringType: StringType[S]): StringType[S] = stringType
  def apply[S](f: String => S, g: S => Option[String]): StringType[S] =
    new StringType[S]:
      def to(s: String): S = f(s)
      def asString(s: S): Option[String] = g(s)
  end apply
end StringType
