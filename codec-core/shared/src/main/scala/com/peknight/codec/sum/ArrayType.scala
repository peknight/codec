package com.peknight.codec.sum

trait ArrayType[S]:
  type Arr
  def to(a: Arr): S
  def asArray(s: S): Option[Arr]
  def fromArray(a: Vector[S]): Arr
  def toArray(a: Arr): Vector[S]
  def isArray(s: S): Boolean = asArray(s).isDefined
end ArrayType
object ArrayType:
  type Aux[S, A] = ArrayType[S] { type Arr = A }
  def apply[S](using arrayType: ArrayType[S]): ArrayType[S] = arrayType
  def apply[S](f: Vector[S] => S, g: S => Option[Vector[S]]): ArrayType[S] =
    new ArrayType[S]:
      type Arr = Vector[S]
      def to(a: Vector[S]): S = f(a)
      def asArray(s: S): Option[Vector[S]] = g(s)
      def fromArray(a: Vector[S]): Vector[S] = a
      def toArray(a: Vector[S]): Vector[S] = a
  end apply
end ArrayType

