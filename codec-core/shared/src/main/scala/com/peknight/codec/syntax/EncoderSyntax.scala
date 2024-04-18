package com.peknight.codec.syntax

import cats.syntax.contravariant.*
import cats.{Contravariant, Id}
import com.peknight.codec.Encoder

trait EncoderSyntax:
  extension [F[_], S] (fs: F[S])
    def encodeTo[A](using contravariant: Contravariant[F], encoder: Encoder[Id, S, A]): F[A] =
      fs.contramap[A](encoder.encode)
    def encodeTo[A](f: F[S] => (A => S) => F[A])(using encoder: Encoder[Id, S, A]): F[A] =
      f(fs)(encoder.encode)
  end extension
end EncoderSyntax
object EncoderSyntax extends EncoderSyntax