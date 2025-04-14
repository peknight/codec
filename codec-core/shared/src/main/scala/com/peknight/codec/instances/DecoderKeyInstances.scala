package com.peknight.codec.instances

import cats.Monad
import com.peknight.codec.Decoder
import com.peknight.codec.reader.{Key, Reader}

trait DecoderKeyInstances:
  given decodeKey[F[_], A](using Decoder[F, String, A], Reader[F, String], Monad[F]): Decoder[F, Key, A] =
    Decoder.decodeK[F, A]
end DecoderKeyInstances
