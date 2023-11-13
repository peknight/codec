package com.peknight.codec.doobie.instances

import com.peknight.generic.migration.id.Migration
import doobie.{Get, Meta}
import org.tpolecat.typename.TypeName

trait GetInstances3 extends GetInstances4:
  extension [T] (meta: Meta[T])
    private[this] def to[A](using migration: Migration[T, A], typeName: TypeName[A]): Get[A] =
      meta.get.tmap(migration.migrate)
  end extension
  given getTypeNameByte[A](using Migration[Byte, A], TypeName[A]): Get[A] = Meta[Byte].to[A]
  given getTypeNameShort[A](using Migration[Short, A], TypeName[A]): Get[A] = Meta[Short].to[A]
  given getTypeNameInt[A](using Migration[Int, A], TypeName[A]): Get[A] = Meta[Int].to[A]
  given getTypeNameLong[A](using Migration[Long, A], TypeName[A]): Get[A] = Meta[Long].to[A]
  given getTypeNameFloat[A](using Migration[Float, A], TypeName[A]): Get[A] = Meta[Float].to[A]
  given getTypeNameDouble[A](using Migration[Double, A], TypeName[A]): Get[A] = Meta[Double].to[A]
  given getTypeNameBigDecimal[A](using Migration[BigDecimal, A], TypeName[A]): Get[A] = Meta[BigDecimal].to[A]
  given getTypeNameBoolean[A](using Migration[Boolean, A], TypeName[A]): Get[A] = Meta[Boolean].to[A]
  given getTypeNameString[A](using Migration[String, A], TypeName[A]): Get[A] = Meta[String].to[A]
  given getTypeNameByteArray[A](using Migration[Array[Byte], A], TypeName[A]): Get[A] = Meta[Array[Byte]].to[A]
end GetInstances3
