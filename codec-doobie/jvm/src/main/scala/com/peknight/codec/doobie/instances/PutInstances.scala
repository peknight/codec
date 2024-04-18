package com.peknight.codec.doobie.instances

import cats.Id
import com.peknight.codec.Encoder
import doobie.{Meta, Put}
import org.tpolecat.typename.TypeName

trait PutInstances extends PutInstances1:
  extension [S] (meta: Meta[S])
    private def encodeTo[A](using encoder: Encoder[Id, S, A], typeName: TypeName[A]): Put[A] =
      meta.put.tcontramap(encoder.encode)(using typeName)
  end extension
  given byteEncodeWithTypeNameAsPut[A](using Encoder[Id, Byte, A], TypeName[A]): Put[A] = Meta[Byte].encodeTo[A]
  given shortEncodeWithTypeNameAsPut[A](using Encoder[Id, Short, A], TypeName[A]): Put[A] = Meta[Short].encodeTo[A]
  given intEncodeWithTypeNameAsPut[A](using Encoder[Id, Int, A], TypeName[A]): Put[A] = Meta[Int].encodeTo[A]
  given longEncodeWithTypeNameAsPut[A](using Encoder[Id, Long, A], TypeName[A]): Put[A] = Meta[Long].encodeTo[A]
  given floatEncodeWithTypeNameAsPut[A](using Encoder[Id, Float, A], TypeName[A]): Put[A] = Meta[Float].encodeTo[A]
  given doubleEncodeWithTypeNameAsPut[A](using Encoder[Id, Double, A], TypeName[A]): Put[A] = Meta[Double].encodeTo[A]
  given bigDecimalEncodeWithTypeNameAsPut[A](using Encoder[Id, BigDecimal, A], TypeName[A]): Put[A] =
    Meta[BigDecimal].encodeTo[A]
  given booleanEncodeWithTypeNameAsPut[A](using Encoder[Id, Boolean, A], TypeName[A]): Put[A] = Meta[Boolean].encodeTo[A]
  given stringEncodeWithTypeNameAsPut[A](using Encoder[Id, String, A], TypeName[A]): Put[A] = Meta[String].encodeTo[A]
  given byteArrayEncodeWithTypeNameAsPut[A](using Encoder[Id, Array[Byte], A], TypeName[A]): Put[A] =
    Meta[Array[Byte]].encodeTo[A]
end PutInstances
