package com.peknight.codec.ciris.syntax

import cats.Id
import ciris.{ConfigValue, Effect}
import com.peknight.codec.Decoder
import com.peknight.codec.ciris.instances.decoder.given

trait ConfigValueSyntax:
  extension (configValue: ConfigValue[Effect, String])
    def asA[A](using Decoder[Id, String, A]): ConfigValue[Effect, A] = configValue.as[A]
  end extension
end ConfigValueSyntax
object ConfigValueSyntax extends ConfigValueSyntax
