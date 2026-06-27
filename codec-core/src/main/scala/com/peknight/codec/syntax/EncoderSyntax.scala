package com.peknight.codec.syntax

import cats.syntax.contravariant.*
import cats.{Contravariant, Id}
import com.peknight.codec.Encoder

trait EncoderSyntax:
  extension [F[_], S] (fs: F[S])
    def encodeTo[A](using contravariant: Contravariant[F], encoder: Encoder[Id, S, A]): F[A] =
      fs.contramap[A](encoder.encode)
  end extension

  extension [A] (a: A)
    def asS[F[_], S](using encoder: Encoder[F, S, A]): F[S] = encoder.encode(a)
  end extension
end EncoderSyntax
object EncoderSyntax extends EncoderSyntax