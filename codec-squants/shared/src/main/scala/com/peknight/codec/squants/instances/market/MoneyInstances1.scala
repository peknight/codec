package com.peknight.codec.squants.instances.market

import cats.{Applicative, Show}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Codec, Decoder, Encoder}
import squants.market.{Money, MoneyContext}

trait MoneyInstances1:
  def numberEncodeMoney0[F[_]](using applicative: Applicative[F], context: MoneyContext)
  : Encoder[F, Number, Money] =
    Encoder.map[F, Number, Money](money => Number.fromBigDecimal(money.to(context.defaultCurrency)))

  given numberDecodeMoney[F[_]](using applicative: Applicative[F], context: MoneyContext): Decoder[F, Number, Money] =
    Decoder.numberDecodeNumberOption[F, Money](_.toBigDecimal.map(context.defaultCurrency.apply))
  given decodeMoneyN[F[_], S](using Applicative[F], NumberType[S], Show[S], MoneyContext)
  : Decoder[F, Cursor[S], Money] = Decoder.decodeN[F, S, Money]
  given stringCodecMoney[F[_]](using applicative: Applicative[F], context: MoneyContext): Codec[F, String, String, Money]
  = Codec.stringCodecWithNumberCodec[F, Money](using numberEncodeMoney0, numberDecodeMoney)
  given codecMoneyS[F[_], S](using Applicative[F], StringType[S], Show[S], MoneyContext)
  : Codec[F, S, Cursor[S], Money] =
    Codec.codecS[F, S, Money]
end MoneyInstances1
