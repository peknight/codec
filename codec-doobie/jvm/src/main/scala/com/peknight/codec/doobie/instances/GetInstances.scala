package com.peknight.codec.doobie.instances

import cats.{Id, Show}
import com.peknight.codec.Decoder
import doobie.{Get, Meta}
import org.tpolecat.typename.TypeName

trait GetInstances extends GetInstances1:
  extension[T] (meta: Meta[T])
    private def decodeTo[A](
      using
      decoder: Decoder[Id, T, A],
      s: Show[T],
      tn: TypeName[T],
      an: TypeName[A]
    ): Get[A] =
      meta.get.temap(t => decoder.decode(t).left.map(_.message))(using s, tn, an)
  end extension
  given byteDecodeWithTypeNameAsGet[A](using Decoder[Id, Byte, A], TypeName[A]): Get[A] =
    Meta[Byte].decodeTo[A]
  given shortDecodeWithTypeNameAsGet[A](using Decoder[Id, Short, A], TypeName[A]): Get[A] =
    Meta[Short].decodeTo[A]
  given intDecodeWithTypeNameAsGet[A](using Decoder[Id, Int, A], TypeName[A]): Get[A] =
    Meta[Int].decodeTo[A]
  given longDecodeWithTypeNameAsGet[A](using Decoder[Id, Long, A], TypeName[A]): Get[A] =
    Meta[Long].decodeTo[A]
  given floatDecodeWithTypeNameAsGet[A](using Decoder[Id, Float, A], TypeName[A]): Get[A] =
    Meta[Float].decodeTo[A]
  given doubleDecodeWithTypeNameAsGet[A](using Decoder[Id, Double, A], TypeName[A]): Get[A] =
    Meta[Double].decodeTo[A]
  given bigDecimalDecodeWithTypeNameAsGet[A](using Decoder[Id, BigDecimal, A], TypeName[A]): Get[A] =
    Meta[BigDecimal].decodeTo[A]
  given booleanDecodeWithTypeNameAsGet[A](using Decoder[Id, Boolean, A], TypeName[A]): Get[A] =
    Meta[Boolean].decodeTo[A]
  given stringDecodeWithTypeNameAsGet[A](using Decoder[Id, String, A], TypeName[A]): Get[A] =
    Meta[String].decodeTo[A]
  given byteArrayDecodeWithTypeNameAsGet[A](using Decoder[Id, Array[Byte], A], TypeName[A]): Get[A] =
    given Show[Array[Byte]] with
      def show(t: Array[Byte]): String = t.mkString(", ")
    end given
    Meta[Array[Byte]].decodeTo[A]
end GetInstances