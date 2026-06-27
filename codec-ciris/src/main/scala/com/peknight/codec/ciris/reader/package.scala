package com.peknight.codec.ciris

import cats.data.Kleisli
import cats.effect.kernel.Async
import ciris.{ConfigValue, Effect}
import com.peknight.codec.reader.{Key, Reader}
import com.peknight.error.syntax.applicativeError.asError

package object reader:
  def apply[F[_]: Async](f: Key => ConfigValue[Effect, String]): Reader[F, String] =
    Kleisli(key => f(key).option.load[F].asError)
end reader
