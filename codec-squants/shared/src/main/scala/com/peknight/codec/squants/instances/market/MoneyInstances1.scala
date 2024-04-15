package com.peknight.codec.squants.instances.market

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.number.Number
import com.peknight.codec.sum.NumberType
import squants.market.{Money, MoneyContext}

trait MoneyInstances1:
  given numberDecodeMoney[F[_]](using applicative: Applicative[F], context: MoneyContext)
  : Decoder[F, Number, DecodingFailure, Money] =
    Decoder.numberDecodeNumberOption[F, Money](_.toBigDecimal.map(context.defaultCurrency.apply))

  given decodeStrictMoney[F[_], S](using applicative: Applicative[F], S: NumberType[S], context: MoneyContext)
  : Decoder[F, Cursor[S], DecodingFailure, Money] =
    Decoder.strictNumberDecoder[F, S, Money]
end MoneyInstances1
