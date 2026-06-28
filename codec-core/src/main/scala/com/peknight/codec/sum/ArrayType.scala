package com.peknight.codec.sum

trait ArrayType[S]:
  def to(a: Vector[S]): S
  def asArray(s: S): Option[Vector[S]]
  def isArray(s: S): Boolean = asArray(s).isDefined
end ArrayType
object ArrayType:
  def apply[S](using arrayType: ArrayType[S]): ArrayType[S] = arrayType
  def apply[S](f: Vector[S] => S, g: S => Option[Vector[S]]): ArrayType[S] =
    new ArrayType[S]:
      def to(a: Vector[S]): S = f(a)
      def asArray(s: S): Option[Vector[S]] = g(s)
  end apply
end ArrayType

