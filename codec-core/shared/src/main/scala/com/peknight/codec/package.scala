package com.peknight

import com.peknight.codec.cursor.Cursor

package object codec:
  type CursorDecoder[F[_], S, E, A] = Decoder[F, Cursor[S], E, A]
  type CursorCodec[F[_], S, E, A] = Codec[F, S, Cursor[S], E, A]
  type IdentityCodec[F[_], S, E, A] = Codec[F, S, S, E, A]
end codec
