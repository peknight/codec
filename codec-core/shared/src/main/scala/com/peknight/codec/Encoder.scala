package com.peknight.codec

import cats.Functor
import cats.syntax.functor.*
import com.peknight.codec.derivation.EncoderDerivationInstances
import com.peknight.codec.instances.{EncoderLowPriorityInstances, EncoderMigrationInstances}

trait Encoder[F[_], S, A]:
  self =>
  def encode(a: A): F[S]
  def contramap[B](f: B => A): Encoder[F, S, B] = (b: B) => self.encode(f(b))
end Encoder
object Encoder extends EncoderMigrationInstances with EncoderDerivationInstances with EncoderLowPriorityInstances:
  trait AsObject[F[_]: Functor, S, A] extends Encoder[F, S, A]:
    type Object
    protected def objectType: ObjectType.Aux[S, Object]
    def encode(a: A): F[S] = encodeObject(a).map(objectType.to)
    def encodeObject(a: A): F[Object]
  end AsObject
  object AsObject:
    type Aux[F[_], S, O, A] = AsObject[F, S, A] { type Object = O }
  end AsObject
end Encoder
