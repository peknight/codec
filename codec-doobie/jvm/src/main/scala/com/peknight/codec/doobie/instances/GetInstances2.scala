package com.peknight.codec.doobie.instances

import cats.Id
import com.peknight.codec.Encoder
import com.peknight.codec.syntax.decoder.decodeTo
import doobie.{Get, Meta}

trait GetInstances2:
  given encodeByteAsGet[A](using Encoder[Id, A, Byte]): Get[A] = Meta[Byte].get.decodeTo[A]
  given encodeShortAsGet[A](using Encoder[Id, A, Short]): Get[A] = Meta[Short].get.decodeTo[A]
  given encodeIntAsGet[A](using Encoder[Id, A, Int]): Get[A] = Meta[Int].get.decodeTo[A]
  given encodeLongAsGet[A](using Encoder[Id, A, Long]): Get[A] = Meta[Long].get.decodeTo[A]
  given encodeFloatAsGet[A](using Encoder[Id, A, Float]): Get[A] = Meta[Float].get.decodeTo[A]
  given encodeDoubleAsGet[A](using Encoder[Id, A, Double]): Get[A] = Meta[Double].get.decodeTo[A]
  given encodeBigDecimalAsGet[A](using Encoder[Id, A, BigDecimal]): Get[A] = Meta[BigDecimal].get.decodeTo[A]
  given encodeBooleanAsGet[A](using Encoder[Id, A, Boolean]): Get[A] = Meta[Boolean].get.decodeTo[A]
  given encodeStringAsGet[A](using Encoder[Id, A, String]): Get[A] = Meta[String].get.decodeTo[A]
  given encodeByteArrayAsGet[A](using Encoder[Id, A, Array[Byte]]): Get[A] = Meta[Array[Byte]].get.decodeTo[A]
end GetInstances2
