package com.peknight.codec.effect

import cats.ApplicativeError
import cats.data.Kleisli
import cats.effect.std.Env
import com.peknight.codec.reader.{Key, Reader}
import com.peknight.error.syntax.applicativeError.asError

package object reader:
  def apply[F[_]](f: Key => String)(using Env[F], ApplicativeError[F, Throwable]): Reader[F, String] =
    Kleisli(key => Env[F].get(f(key)).asError)
end reader