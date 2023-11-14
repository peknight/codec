package com.peknight.codec.doobie.instances

import com.peknight.codec.id.Encoder
import com.peknight.codec.syntax.decoder.decodeTo
import doobie.{Get, Meta}

trait GetEncoderInstances:
  given byteEncoderGet[A](using Encoder[A, Byte]): Get[A] = Meta[Byte].get.decodeTo[A]
  given shortEncoderGet[A](using Encoder[A, Short]): Get[A] = Meta[Short].get.decodeTo[A]
  given intEncoderGet[A](using Encoder[A, Int]): Get[A] = Meta[Int].get.decodeTo[A]
  given longEncoderGet[A](using Encoder[A, Long]): Get[A] = Meta[Long].get.decodeTo[A]
  given floatEncoderGet[A](using Encoder[A, Float]): Get[A] = Meta[Float].get.decodeTo[A]
  given doubleEncoderGet[A](using Encoder[A, Double]): Get[A] = Meta[Double].get.decodeTo[A]
  given bigDecimalEncoderGet[A](using Encoder[A, BigDecimal]): Get[A] = Meta[BigDecimal].get.decodeTo[A]
  given booleanEncoderGet[A](using Encoder[A, Boolean]): Get[A] = Meta[Boolean].get.decodeTo[A]
  given stringEncoderGet[A](using Encoder[A, String]): Get[A] = Meta[String].get.decodeTo[A]
  given byteArrayEncoderGet[A](using Encoder[A, Array[Byte]]): Get[A] = Meta[Array[Byte]].get.decodeTo[A]
end GetEncoderInstances
