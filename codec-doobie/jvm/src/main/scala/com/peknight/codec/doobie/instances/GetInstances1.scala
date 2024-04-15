package com.peknight.codec.doobie.instances

import com.peknight.codec.id.Encoder
import doobie.{Get, Meta}
import org.tpolecat.typename.TypeName

trait GetInstances1 extends GetInstances2:
  extension [T] (meta: Meta[T])
    private[this] def decodeTo[A](using encoder: Encoder[A, T], typeName: TypeName[A]): Get[A] =
      meta.get.tmap(encoder.encode)(using typeName)
  end extension
  given byteTypeNameEncoderGet[A](using Encoder[A, Byte], TypeName[A]): Get[A] = Meta[Byte].decodeTo[A]
  given shortTypeNameEncoderGet[A](using Encoder[A, Short], TypeName[A]): Get[A] = Meta[Short].decodeTo[A]
  given intTypeNameEncoderGet[A](using Encoder[A, Int], TypeName[A]): Get[A] = Meta[Int].decodeTo[A]
  given longTypeNameEncoderGet[A](using Encoder[A, Long], TypeName[A]): Get[A] = Meta[Long].decodeTo[A]
  given floatTypeNameEncoderGet[A](using Encoder[A, Float], TypeName[A]): Get[A] = Meta[Float].decodeTo[A]
  given doubleTypeNameEncoderGet[A](using Encoder[A, Double], TypeName[A]): Get[A] = Meta[Double].decodeTo[A]
  given bigDecimalTypeNameEncoderGet[A](using Encoder[A, BigDecimal], TypeName[A]): Get[A] =
    Meta[BigDecimal].decodeTo[A]
  given booleanTypeNameEncoderGet[A](using Encoder[A, Boolean], TypeName[A]): Get[A] = Meta[Boolean].decodeTo[A]
  given stringTypeNameEncoderGet[A](using Encoder[A, String], TypeName[A]): Get[A] = Meta[String].decodeTo[A]
  given byteArrayTypeNameEncoderGet[A](using Encoder[A, Array[Byte]], TypeName[A]): Get[A] =
    Meta[Array[Byte]].decodeTo[A]
end GetInstances1
