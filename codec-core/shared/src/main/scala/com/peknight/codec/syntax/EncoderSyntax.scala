package com.peknight.codec.syntax

import cats.Contravariant
import cats.syntax.contravariant.*
import com.peknight.codec.id.Encoder

trait EncoderSyntax:
  extension [F[_], S] (fs: F[S])
    def encodeTo[A](using encoder: Encoder[S, A], contravariant: Contravariant[F]): F[A] =
      fs.contramap[A](encoder.encode)
    def encodeTo[A](f: F[S] => (A => S) => F[A])(using encoder: Encoder[S, A]): F[A] =
      f(fs)(encoder.encode)
  end extension
end EncoderSyntax
object EncoderSyntax extends EncoderSyntax