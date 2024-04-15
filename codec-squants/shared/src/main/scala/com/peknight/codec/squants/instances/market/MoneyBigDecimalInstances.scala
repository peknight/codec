package com.peknight.codec.squants.instances.market

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.{Decoder, Encoder}
import squants.market.{Money, MoneyContext}

trait MoneyBigDecimalInstances:
  given bigDecimalMoneyEncoder[F[_]](using applicative: Applicative[F], context: MoneyContext): Encoder[F, BigDecimal, Money] =
    Encoder.instance[F, BigDecimal, Money](a => a.to(context.defaultCurrency).pure[F])

  given bigDecimalMoneyDecoder[F[_]](using applicative: Applicative[F], context: MoneyContext)
  : Decoder[F, BigDecimal, DecodingFailure, Money] =
    Decoder.instance[F, BigDecimal, DecodingFailure, Money](t =>
      context.defaultCurrency(t).asRight[DecodingFailure].pure[F]
    )
end MoneyBigDecimalInstances
object MoneyBigDecimalInstances extends MoneyBigDecimalInstances
