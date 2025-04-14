package com.peknight.codec

import cats.data.ReaderT
import com.peknight.error.Error

package object reader:
  type Reader[F[_], A] = ReaderT[[X] =>> F[Either[Error, Option[X]]], Key, A]
end reader
