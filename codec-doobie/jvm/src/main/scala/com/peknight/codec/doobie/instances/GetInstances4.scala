package com.peknight.codec.doobie.instances

import com.peknight.generic.migration.id.Migration
import doobie.{Get, Meta}

trait GetInstances4:
  extension [T] (meta: Meta[T])
    private[this] def to[A](using migration: Migration[T, A]): Get[A] = meta.get.map(migration.migrate)
  end extension
  given getByte[A](using Migration[Byte, A]): Get[A] = Meta[Byte].to[A]
  given getShort[A](using Migration[Short, A]): Get[A] = Meta[Short].to[A]
  given getInt[A](using Migration[Int, A]): Get[A] = Meta[Int].to[A]
  given getLong[A](using Migration[Long, A]): Get[A] = Meta[Long].to[A]
  given getFloat[A](using Migration[Float, A]): Get[A] = Meta[Float].to[A]
  given getDouble[A](using Migration[Double, A]): Get[A] = Meta[Double].to[A]
  given getBigDecimal[A](using Migration[BigDecimal, A]): Get[A] = Meta[BigDecimal].to[A]
  given getBoolean[A](using Migration[Boolean, A]): Get[A] = Meta[Boolean].to[A]
  given getString[A](using Migration[String, A]): Get[A] = Meta[String].to[A]
  given getByteArray[A](using Migration[Array[Byte], A]): Get[A] = Meta[Array[Byte]].to[A]
end GetInstances4
