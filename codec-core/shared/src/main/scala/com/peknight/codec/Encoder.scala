package com.peknight.codec

import com.peknight.codec.derivation.EncoderDerivation
import com.peknight.codec.instances.EncoderMigrationInstances
import com.peknight.generic.priority.PriorityInstances

trait Encoder[F[_], S, A]:
  self =>
  def encode(a: A): F[S]
  def contramap[B](f: B => A): Encoder[F, S, B] = (b: B) => self.encode(f(b))
end Encoder
object Encoder extends EncoderMigrationInstances with EncoderDerivation with PriorityInstances
