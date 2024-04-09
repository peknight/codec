package com.peknight.codec.sum

import com.peknight.codec.number.Number

trait NumberType[S]:
  def to(n: Number): S
  def asNumber(s: S): Option[Number]
  def isNumber(s: S): Boolean = asNumber(s).isDefined
end NumberType
object NumberType:
  def apply[S](using numberType: NumberType[S]): NumberType[S] = numberType
  def apply[S](f: Number => S, g: S => Option[Number]): NumberType[S] =
    new NumberType[S]:
      def to(n: Number): S = f(n)
      def asNumber(s: S): Option[Number] = g(s)
  end apply
end NumberType
