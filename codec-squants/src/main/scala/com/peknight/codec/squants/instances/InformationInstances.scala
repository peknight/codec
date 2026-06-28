package com.peknight.codec.squants.instances

import cats.{Applicative, Show}
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
import squants.information.Information

trait InformationInstances:
  given stringCodecInformation[F[_]: Applicative]: Codec[F, String, String, Information] =
    Codec.mapTry[F, String, String, Information](_.toString)(Information.parseString)

  given codecInformationS[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], Information] =
    Codec.codecS[F, S, Information]
end InformationInstances
object InformationInstances extends InformationInstances
