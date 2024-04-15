package com.peknight.codec.doobie.instances

import com.peknight.codec.id.Encoder
import com.peknight.codec.syntax.encoder.encodeTo
import doobie.{Meta, Put}

trait PutInstances1:
  given byteEncoderPut[A](using encoder: Encoder[Byte, A]): Put[A] = Meta[Byte].put.encodeTo[A]
  given shortEncoderPut[A](using encoder: Encoder[Short, A]): Put[A] = Meta[Short].put.encodeTo[A]
  given intEncoderPut[A](using encoder: Encoder[Int, A]): Put[A] = Meta[Int].put.encodeTo[A]
  given longEncoderPut[A](using encoder: Encoder[Long, A]): Put[A] = Meta[Long].put.encodeTo[A]
  given floatEncoderPut[A](using encoder: Encoder[Float, A]): Put[A] = Meta[Float].put.encodeTo[A]
  given doubleEncoderPut[A](using encoder: Encoder[Double, A]): Put[A] = Meta[Double].put.encodeTo[A]
  given bigDecimalEncoderPut[A](using encoder: Encoder[BigDecimal, A]): Put[A] = Meta[BigDecimal].put.encodeTo[A]
  given booleanEncoderPut[A](using encoder: Encoder[Boolean, A]): Put[A] = Meta[Boolean].put.encodeTo[A]
  given stringEncoderPut[A](using encoder: Encoder[String, A]): Put[A] = Meta[String].put.encodeTo[A]
  given byteArrayEncoderPut[A](using encoder: Encoder[Array[Byte], A]): Put[A] = Meta[Array[Byte]].put.encodeTo[A]
end PutInstances1
