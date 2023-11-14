package com.peknight.codec.doobie.instances

import cats.Show
import com.peknight.codec.id.Decoder
import doobie.{Get, Meta}
import org.tpolecat.typename.TypeName

trait GetTypeNameDecoderInstances:
  extension[T] (meta: Meta[T])
    private[this] def to[A](using decoder: Decoder[T, String, A], s: Show[T], tn: TypeName[T], an: TypeName[A]): Get[A] =
      meta.get.temap(t => decoder.decode(t))(using s, tn, an)
  end extension
  given byteTypeNameDecoderGet[A](using Decoder[Byte, String, A], TypeName[A]): Get[A] = Meta[Byte].to[A]
  given shortTypeNameDecoderGet[A](using Decoder[Short, String, A], TypeName[A]): Get[A] = Meta[Short].to[A]
  given intTypeNameDecoderGet[A](using Decoder[Int, String, A], TypeName[A]): Get[A] = Meta[Int].to[A]
  given longTypeNameDecoderGet[A](using Decoder[Long, String, A], TypeName[A]): Get[A] = Meta[Long].to[A]
  given floatTypeNameDecoderGet[A](using Decoder[Float, String, A], TypeName[A]): Get[A] = Meta[Float].to[A]
  given doubleTypeNameDecoderGet[A](using Decoder[Double, String, A], TypeName[A]): Get[A] = Meta[Double].to[A]
  given bigDecimalTypeNameDecoderGet[A](using Decoder[BigDecimal, String, A], TypeName[A]): Get[A] =
    Meta[BigDecimal].to[A]
  given booleanTypeNameDecoderGet[A](using Decoder[Boolean, String, A], TypeName[A]): Get[A] = Meta[Boolean].to[A]
  given stringTypeNameDecoderGet[A](using Decoder[String, String, A], TypeName[A]): Get[A] = Meta[String].to[A]
  given byteArrayTypeNameDecoderGet[A](using Decoder[Array[Byte], String, A], TypeName[A]): Get[A] =
    given Show[Array[Byte]] with
      def show(t: Array[Byte]): String = t.mkString(", ")
    end given
    Meta[Array[Byte]].to[A]
end GetTypeNameDecoderInstances