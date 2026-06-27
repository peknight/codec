package com.peknight.codec.ciris.instances

import cats.effect.kernel.Async
import ciris.{env, prop}
import com.peknight.codec.ciris.reader
import com.peknight.codec.reader.Reader
import com.peknight.commons.text.cases.{ScreamingSnakeCase, split}

trait EnvPropsReaderInstances:
  given envPropsReader[F[_]: Async]: Reader[F, String] =
    reader(key => env(ScreamingSnakeCase.join(key.keys.toSeq.flatMap(split))).or(prop(key.keys.mkString("."))))
end EnvPropsReaderInstances
object EnvPropsReaderInstances extends EnvPropsReaderInstances
