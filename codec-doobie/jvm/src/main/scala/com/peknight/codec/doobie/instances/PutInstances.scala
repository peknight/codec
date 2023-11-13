package com.peknight.codec.doobie.instances

import com.peknight.codec.id.Encoder
import doobie.{Meta, Put}
import org.tpolecat.typename.TypeName

trait PutInstances extends PutInstances2:
  extension [S] (meta: Meta[S])
    private def to[A](using encoder: Encoder[S, A], typeName: TypeName[A]): Put[A] =
      meta.put.tcontramap(encoder.encode)
  end extension
  given putTypeNameByte[A](using Encoder[Byte, A], TypeName[A]): Put[A] = Meta[Byte].to[A]
  given putTypeNameShort[A](using Encoder[Short, A], TypeName[A]): Put[A] = Meta[Short].to[A]
  given putTypeNameInt[A](using Encoder[Int, A], TypeName[A]): Put[A] = Meta[Int].to[A]
  given putTypeNameLong[A](using Encoder[Long, A], TypeName[A]): Put[A] = Meta[Long].to[A]
  given putTypeNameFloat[A](using Encoder[Float, A], TypeName[A]): Put[A] = Meta[Float].to[A]
  given putTypeNameDouble[A](using Encoder[Double, A], TypeName[A]): Put[A] = Meta[Double].to[A]
  given putTypeNameBigDecimal[A](using Encoder[BigDecimal, A], TypeName[A]): Put[A] = Meta[BigDecimal].to[A]
  given putTypeNameBoolean[A](using Encoder[Boolean, A], TypeName[A]): Put[A] = Meta[Boolean].to[A]
  given putTypeNameString[A](using Encoder[String, A], TypeName[A]): Put[A] = Meta[String].to[A]
  given putTypeNameByteArray[A](using Encoder[Array[Byte], A], TypeName[A]): Put[A] = Meta[Array[Byte]].to[A]
end PutInstances
object PutInstances extends PutInstances
