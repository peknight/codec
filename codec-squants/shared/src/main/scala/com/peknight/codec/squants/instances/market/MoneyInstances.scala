package com.peknight.codec.squants.instances.market

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Decoder, Encoder}
import squants.market.{Money, MoneyContext}

trait MoneyInstances extends MoneyInstances1:
  given numberEncodeMoney[F[_]](using applicative: Applicative[F], context: MoneyContext): Encoder[F, Number, Money] =
    numberEncodeMoney0[F]

  given encodeMoneyN[F[_], S](using applicative: Applicative[F], numberType: NumberType[S], context: MoneyContext)
  : Encoder[F, S, Money] =
    Encoder.encodeN[F, S, Money]

  given decodeMoneyNS[F[_], S](using applicative: Applicative[F], numberType: NumberType[S], stringType: StringType[S],
                               context: MoneyContext): Decoder[F, Cursor[S], Money] =
    Decoder.decodeNS[F, S, Money]
end MoneyInstances
object MoneyInstances extends MoneyInstances
