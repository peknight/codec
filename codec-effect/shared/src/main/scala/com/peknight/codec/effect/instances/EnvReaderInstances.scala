package com.peknight.codec.effect.instances

import cats.ApplicativeError
import cats.effect.std.Env
import com.peknight.codec.effect.reader
import com.peknight.codec.reader.Reader
import com.peknight.commons.text.cases.{ScreamingSnakeCase, split}

trait EnvReaderInstances:
  given envReader[F[_]](using Env[F], ApplicativeError[F, Throwable]): Reader[F, String] =
    reader(key => ScreamingSnakeCase.join(key.keys.toSeq.flatMap(split)))
end EnvReaderInstances
object EnvReaderInstances extends EnvReaderInstances
