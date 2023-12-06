package com.peknight.codec.sum

trait NullType[S]:
  def unit: S
  def asNull(s: S): Option[Unit]
  def isNull(s: S): Boolean = asNull(s).isDefined
end NullType
object NullType:
  def apply[S](using nullType: NullType[S]): NullType[S] = nullType
  def nullType[S](f: => S, g: S => Option[Unit]): NullType[S] =
    new NullType[S]:
      def unit: S = f
      def asNull(s: S): Option[Unit] = g(s)
  end nullType
end NullType
