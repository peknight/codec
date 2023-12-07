package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.codec.cursor.{Cursor, Decoder, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, NotObject}
import com.peknight.codec.sum.ObjectType

trait DecoderObjectInstances:
  given decodeObject[F[_], S, O](using applicative: Applicative[F], objectType: ObjectType.Aux[S, O])
  : Decoder[F, S, O] with
    def apply(t: SuccessCursor[S]): F[Either[DecodingFailure[Cursor[S]], O]] =
      objectType.asObject(t.value) match
        case Some(o) => o.asRight.pure
        case None => NotObject(t).asLeft.pure
  end decodeObject
end DecoderObjectInstances
