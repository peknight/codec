package com.peknight.codec

import com.peknight.codec.derivation.EncoderDerivationInstances
import com.peknight.codec.instances.{EncoderLowPriorityInstances, EncoderMigrationInstances}

trait Encoder[F[_], S, A]:
  self =>
  def encode(a: A): F[S]
  def contramap[B](f: B => A): Encoder[F, S, B] = (b: B) => self.encode(f(b))
end Encoder
object Encoder extends EncoderMigrationInstances with EncoderDerivationInstances with EncoderLowPriorityInstances
