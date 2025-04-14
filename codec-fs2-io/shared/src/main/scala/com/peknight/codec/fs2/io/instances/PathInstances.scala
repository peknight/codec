package com.peknight.codec.fs2.io.instances

import cats.Applicative
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
import fs2.io.file.Path

trait PathInstances:
  given stringCodecPath[F[_]: Applicative]: Codec[F, String, String, Path] =
    Codec.map[F, String, String, Path](_.toString)(Path.apply)

  given codecPathS[F[_]: Applicative, S: StringType]: Codec[F, S, Cursor[S], Path] = Codec.codecS[F, S, Path]
end PathInstances
object PathInstances extends PathInstances
