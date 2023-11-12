package com.peknight.codec

import cats.Id

package object id:
  type Codec[S, T, E, A] = com.peknight.codec.Codec[Id, S, T, E, A]
  type CursorCodec[S, E, A] = com.peknight.codec.CursorCodec[Id, S, E, A]
  type IdentityCodec[S, E, A] = com.peknight.codec.IdentityCodec[Id, S, E, A]
  type Encoder[S, A] = com.peknight.codec.Encoder[Id, S, A]
  type Decoder[T, E, A] = com.peknight.codec.Decoder[Id, T, E, A]
end id
