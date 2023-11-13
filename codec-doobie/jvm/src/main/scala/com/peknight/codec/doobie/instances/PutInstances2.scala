package com.peknight.codec.doobie.instances

import com.peknight.codec.id.Encoder
import doobie.{Meta, Put}

trait PutInstances2:
  extension [S] (meta: Meta[S])
    private def to[A](using encoder: Encoder[S, A]): Put[A] = meta.put.contramap(encoder.encode)
  end extension
  given putByte[A](using encoder: Encoder[Byte, A]): Put[A] = Meta[Byte].to[A]
  given putShort[A](using encoder: Encoder[Short, A]): Put[A] = Meta[Short].to[A]
  given putInt[A](using encoder: Encoder[Int, A]): Put[A] = Meta[Int].to[A]
  given putLong[A](using encoder: Encoder[Long, A]): Put[A] = Meta[Long].to[A]
  given putFloat[A](using encoder: Encoder[Float, A]): Put[A] = Meta[Float].to[A]
  given putDouble[A](using encoder: Encoder[Double, A]): Put[A] = Meta[Double].to[A]
  given putBigDecimal[A](using encoder: Encoder[BigDecimal, A]): Put[A] = Meta[BigDecimal].to[A]
  given putBoolean[A](using encoder: Encoder[Boolean, A]): Put[A] = Meta[Boolean].to[A]
  given putString[A](using encoder: Encoder[String, A]): Put[A] = Meta[String].to[A]
  given putByteArray[A](using encoder: Encoder[Array[Byte], A]): Put[A] = Meta[Array[Byte]].to[A]
end PutInstances2
