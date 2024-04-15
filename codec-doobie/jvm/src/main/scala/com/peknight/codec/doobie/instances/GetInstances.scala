package com.peknight.codec.doobie.instances

import cats.Show
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.id.Decoder
import doobie.{Get, Meta}
import org.tpolecat.typename.TypeName

trait GetInstances extends GetInstances1:
  extension[T] (meta: Meta[T])
    private[this] def decodeTo[A](
      using
      decoder: Decoder[T, DecodingFailure, A],
      s: Show[T],
      tn: TypeName[T],
      an: TypeName[A]
    ): Get[A] =
      meta.get.temap(t => decoder.decode(t).left.map(_.message))(using s, tn, an)
  end extension
  given byteTypeNameDecoderGet[A](using Decoder[Byte, DecodingFailure, A], TypeName[A]): Get[A] =
    Meta[Byte].decodeTo[A]
  given shortTypeNameDecoderGet[A](using Decoder[Short, DecodingFailure, A], TypeName[A]): Get[A] =
    Meta[Short].decodeTo[A]
  given intTypeNameDecoderGet[A](using Decoder[Int, DecodingFailure, A], TypeName[A]): Get[A] =
    Meta[Int].decodeTo[A]
  given longTypeNameDecoderGet[A](using Decoder[Long, DecodingFailure, A], TypeName[A]): Get[A] =
    Meta[Long].decodeTo[A]
  given floatTypeNameDecoderGet[A](using Decoder[Float, DecodingFailure, A], TypeName[A]): Get[A] =
    Meta[Float].decodeTo[A]
  given doubleTypeNameDecoderGet[A](using Decoder[Double, DecodingFailure, A], TypeName[A]): Get[A] =
    Meta[Double].decodeTo[A]
  given bigDecimalTypeNameDecoderGet[A](using Decoder[BigDecimal, DecodingFailure, A], TypeName[A]): Get[A] =
    Meta[BigDecimal].decodeTo[A]
  given booleanTypeNameDecoderGet[A](using Decoder[Boolean, DecodingFailure, A], TypeName[A]): Get[A] =
    Meta[Boolean].decodeTo[A]
  given stringTypeNameDecoderGet[A](using Decoder[String, DecodingFailure, A], TypeName[A]): Get[A] =
    Meta[String].decodeTo[A]
  given byteArrayTypeNameDecoderGet[A](using Decoder[Array[Byte], DecodingFailure, A], TypeName[A]): Get[A] =
    given Show[Array[Byte]] with
      def show(t: Array[Byte]): String = t.mkString(", ")
    end given
    Meta[Array[Byte]].decodeTo[A]
end GetInstances