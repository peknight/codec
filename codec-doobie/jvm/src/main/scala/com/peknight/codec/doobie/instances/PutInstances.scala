package com.peknight.codec.doobie.instances

import com.peknight.codec.id.Encoder
import doobie.{Meta, Put}
import org.tpolecat.typename.TypeName

trait PutInstances extends PutInstances1:
  extension [S] (meta: Meta[S])
    private def encodeTo[A](using encoder: Encoder[S, A], typeName: TypeName[A]): Put[A] =
      meta.put.tcontramap(encoder.encode)(using typeName)
  end extension
  given byteTypeNameEncoderPut[A](using Encoder[Byte, A], TypeName[A]): Put[A] = Meta[Byte].encodeTo[A]
  given shortTypeNameEncoderPut[A](using Encoder[Short, A], TypeName[A]): Put[A] = Meta[Short].encodeTo[A]
  given intTypeNameEncoderPut[A](using Encoder[Int, A], TypeName[A]): Put[A] = Meta[Int].encodeTo[A]
  given longTypeNameEncoderPut[A](using Encoder[Long, A], TypeName[A]): Put[A] = Meta[Long].encodeTo[A]
  given floatTypeNameEncoderPut[A](using Encoder[Float, A], TypeName[A]): Put[A] = Meta[Float].encodeTo[A]
  given doubleTypeNameEncoderPut[A](using Encoder[Double, A], TypeName[A]): Put[A] = Meta[Double].encodeTo[A]
  given bigDecimalTypeNameEncoderPut[A](using Encoder[BigDecimal, A], TypeName[A]): Put[A] = Meta[BigDecimal].encodeTo[A]
  given booleanTypeNameEncoderPut[A](using Encoder[Boolean, A], TypeName[A]): Put[A] = Meta[Boolean].encodeTo[A]
  given stringTypeNameEncoderPut[A](using Encoder[String, A], TypeName[A]): Put[A] = Meta[String].encodeTo[A]
  given byteArrayTypeNameEncoderPut[A](using Encoder[Array[Byte], A], TypeName[A]): Put[A] = Meta[Array[Byte]].encodeTo[A]
end PutInstances
