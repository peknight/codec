package com.peknight.codec.doobie.instances

import cats.Show
import com.peknight.codec.id.Decoder
import doobie.{Get, Meta}
import com.peknight.error.Error
import org.tpolecat.typename.TypeName

trait GetInstances2 extends GetInstances3:
  extension [T] (meta: Meta[T])
    private[this] def to[A](using decoder: Decoder[T, Error, A], s: Show[T], tn: TypeName[T], an: TypeName[A]): Get[A] =
      meta.get.temap(t => decoder.decode(t).left.map(_.message))
  end extension
  given getTypeNameErrorByte[A](using Decoder[Byte, Error, A], TypeName[A]): Get[A] = Meta[Byte].to[A]
  given getTypeNameErrorShort[A](using Decoder[Short, Error, A], TypeName[A]): Get[A] = Meta[Short].to[A]
  given getTypeNameErrorInt[A](using Decoder[Int, Error, A], TypeName[A]): Get[A] = Meta[Int].to[A]
  given getTypeNameErrorLong[A](using Decoder[Long, Error, A], TypeName[A]): Get[A] = Meta[Long].to[A]
  given getTypeNameErrorFloat[A](using Decoder[Float, Error, A], TypeName[A]): Get[A] = Meta[Float].to[A]
  given getTypeNameErrorDouble[A](using Decoder[Double, Error, A], TypeName[A]): Get[A] = Meta[Double].to[A]
  given getTypeNameErrorBigDecimal[A](using Decoder[BigDecimal, Error, A], TypeName[A]): Get[A] = Meta[BigDecimal].to[A]
  given getTypeNameErrorBoolean[A](using Decoder[Boolean, Error, A], TypeName[A]): Get[A] = Meta[Boolean].to[A]
  given getTypeNameErrorString[A](using Decoder[String, Error, A], TypeName[A]): Get[A] = Meta[String].to[A]
  protected[instances] given Show[Array[Byte]] with
    def show(t: Array[Byte]): String = t.mkString(", ")
  end given
  given getTypeNameErrorByteArray[A](using Decoder[Array[Byte], Error, A], TypeName[A]): Get[A] =
    Meta[Array[Byte]].to[A]
end GetInstances2
