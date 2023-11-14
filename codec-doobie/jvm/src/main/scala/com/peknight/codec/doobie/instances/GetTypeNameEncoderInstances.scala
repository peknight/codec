package com.peknight.codec.doobie.instances

import com.peknight.codec.id.Encoder
import doobie.{Get, Meta}
import org.tpolecat.typename.TypeName

trait GetTypeNameEncoderInstances:
  extension [T] (meta: Meta[T])
    private[this] def to[A](using encoder: Encoder[A, T], typeName: TypeName[A]): Get[A] =
      meta.get.tmap(encoder.encode)(using typeName)
  end extension
  given byteTypeNameEncoderGet[A](using Encoder[A, Byte], TypeName[A]): Get[A] = Meta[Byte].to[A]
  given shortTypeNameEncoderGet[A](using Encoder[A, Short], TypeName[A]): Get[A] = Meta[Short].to[A]
  given intTypeNameEncoderGet[A](using Encoder[A, Int], TypeName[A]): Get[A] = Meta[Int].to[A]
  given longTypeNameEncoderGet[A](using Encoder[A, Long], TypeName[A]): Get[A] = Meta[Long].to[A]
  given floatTypeNameEncoderGet[A](using Encoder[A, Float], TypeName[A]): Get[A] = Meta[Float].to[A]
  given doubleTypeNameEncoderGet[A](using Encoder[A, Double], TypeName[A]): Get[A] = Meta[Double].to[A]
  given bigDecimalTypeNameEncoderGet[A](using Encoder[A, BigDecimal], TypeName[A]): Get[A] = Meta[BigDecimal].to[A]
  given booleanTypeNameEncoderGet[A](using Encoder[A, Boolean], TypeName[A]): Get[A] = Meta[Boolean].to[A]
  given stringTypeNameEncoderGet[A](using Encoder[A, String], TypeName[A]): Get[A] = Meta[String].to[A]
  given byteArrayTypeNameEncoderGet[A](using Encoder[A, Array[Byte]], TypeName[A]): Get[A] = Meta[Array[Byte]].to[A]
end GetTypeNameEncoderInstances
