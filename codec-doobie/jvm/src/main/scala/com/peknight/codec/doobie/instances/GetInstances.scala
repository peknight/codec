package com.peknight.codec.doobie.instances

import cats.Show
import com.peknight.codec.id.Decoder
import doobie.{Get, Meta}
import org.tpolecat.typename.TypeName
trait GetInstances extends GetInstances2:
  extension[T] (meta: Meta[T])
    private[this] def to[A](using decoder: Decoder[T, String, A], s: Show[T], tn: TypeName[T], an: TypeName[A]): Get[A] =
      meta.get.temap(t => decoder.decode(t))
  end extension
  given getTypeNameStringByte[A](using Decoder[Byte, String, A], TypeName[A]): Get[A] = Meta[Byte].to[A]
  given getTypeNameStringShort[A](using Decoder[Short, String, A], TypeName[A]): Get[A] = Meta[Short].to[A]
  given getTypeNameStringInt[A](using Decoder[Int, String, A], TypeName[A]): Get[A] = Meta[Int].to[A]
  given getTypeNameStringLong[A](using Decoder[Long, String, A], TypeName[A]): Get[A] = Meta[Long].to[A]
  given getTypeNameStringFloat[A](using Decoder[Float, String, A], TypeName[A]): Get[A] = Meta[Float].to[A]
  given getTypeNameStringDouble[A](using Decoder[Double, String, A], TypeName[A]): Get[A] = Meta[Double].to[A]
  given getTypeNameStringBigDecimal[A](using Decoder[BigDecimal, String, A], TypeName[A]): Get[A] =
    Meta[BigDecimal].to[A]
  given getTypeNameStringBoolean[A](using Decoder[Boolean, String, A], TypeName[A]): Get[A] = Meta[Boolean].to[A]
  given getTypeNameStringString[A](using Decoder[String, String, A], TypeName[A]): Get[A] = Meta[String].to[A]
  given getTypeNameStringByteArray[A](using Decoder[Array[Byte], String, A], TypeName[A]): Get[A] =
    Meta[Array[Byte]].to[A]
end GetInstances
object GetInstances extends GetInstances
