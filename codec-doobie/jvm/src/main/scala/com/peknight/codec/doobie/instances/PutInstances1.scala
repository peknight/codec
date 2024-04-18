package com.peknight.codec.doobie.instances

import cats.Id
import com.peknight.codec.Encoder
import com.peknight.codec.syntax.encoder.encodeTo
import doobie.{Meta, Put}

trait PutInstances1:
  given byteEncoderAsPut[A](using encoder: Encoder[Id, Byte, A]): Put[A] = Meta[Byte].put.encodeTo[A]
  given shortEncoderAsPut[A](using encoder: Encoder[Id, Short, A]): Put[A] = Meta[Short].put.encodeTo[A]
  given intEncoderAsPut[A](using encoder: Encoder[Id, Int, A]): Put[A] = Meta[Int].put.encodeTo[A]
  given longEncoderAsPut[A](using encoder: Encoder[Id, Long, A]): Put[A] = Meta[Long].put.encodeTo[A]
  given floatEncoderAsPut[A](using encoder: Encoder[Id, Float, A]): Put[A] = Meta[Float].put.encodeTo[A]
  given doubleEncoderAsPut[A](using encoder: Encoder[Id, Double, A]): Put[A] = Meta[Double].put.encodeTo[A]
  given bigDecimalEncoderAsPut[A](using encoder: Encoder[Id, BigDecimal, A]): Put[A] = Meta[BigDecimal].put.encodeTo[A]
  given booleanEncoderAsPut[A](using encoder: Encoder[Id, Boolean, A]): Put[A] = Meta[Boolean].put.encodeTo[A]
  given stringEncoderAsPut[A](using encoder: Encoder[Id, String, A]): Put[A] = Meta[String].put.encodeTo[A]
  given byteArrayEncoderAsPut[A](using encoder: Encoder[Id, Array[Byte], A]): Put[A] = Meta[Array[Byte]].put.encodeTo[A]
end PutInstances1
