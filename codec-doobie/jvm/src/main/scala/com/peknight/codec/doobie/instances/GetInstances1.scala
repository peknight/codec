package com.peknight.codec.doobie.instances

import cats.Id
import com.peknight.codec.Encoder
import doobie.{Get, Meta}
import org.tpolecat.typename.TypeName

trait GetInstances1 extends GetInstances2:
  extension [T] (meta: Meta[T])
    private[this] def decodeTo[A](using encoder: Encoder[Id, A, T], typeName: TypeName[A]): Get[A] =
      meta.get.tmap(encoder.encode)(using typeName)
  end extension
  given encodeByteWithTypeNameAsGet[A](using Encoder[Id, A, Byte], TypeName[A]): Get[A] = Meta[Byte].decodeTo[A]
  given encodeShortWithTypeNameAsGet[A](using Encoder[Id, A, Short], TypeName[A]): Get[A] = Meta[Short].decodeTo[A]
  given encodeIntWithTypeNameAsGet[A](using Encoder[Id, A, Int], TypeName[A]): Get[A] = Meta[Int].decodeTo[A]
  given encodeLongWithTypeNameAsGet[A](using Encoder[Id, A, Long], TypeName[A]): Get[A] = Meta[Long].decodeTo[A]
  given encodeFloatWithTypeNameAsGet[A](using Encoder[Id, A, Float], TypeName[A]): Get[A] = Meta[Float].decodeTo[A]
  given encodeDoubleWithTypeNameAsGet[A](using Encoder[Id, A, Double], TypeName[A]): Get[A] = Meta[Double].decodeTo[A]
  given encodeBigDecimalWithTypeNameAsGet[A](using Encoder[Id, A, BigDecimal], TypeName[A]): Get[A] =
    Meta[BigDecimal].decodeTo[A]
  given encodeBooleanWithTypeNameAsGet[A](using Encoder[Id, A, Boolean], TypeName[A]): Get[A] = Meta[Boolean].decodeTo[A]
  given encodeStringWithTypeNameAsGet[A](using Encoder[Id, A, String], TypeName[A]): Get[A] = Meta[String].decodeTo[A]
  given encodeByteArrayWithTypeNameAsGet[A](using Encoder[Id, A, Array[Byte]], TypeName[A]): Get[A] =
    Meta[Array[Byte]].decodeTo[A]
end GetInstances1
