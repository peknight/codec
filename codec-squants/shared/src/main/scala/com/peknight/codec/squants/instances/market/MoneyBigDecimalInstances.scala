package com.peknight.codec.squants.instances.market

import cats.Applicative
import com.peknight.codec.Codec
import squants.market.{Money, MoneyContext}

trait MoneyBigDecimalInstances:
  given bigDecimalCodecMoney[F[_]](using applicative: Applicative[F], context: MoneyContext)
  : Codec[F, BigDecimal, BigDecimal, Money] =
    Codec.map[F, BigDecimal, BigDecimal, Money](_.to(context.defaultCurrency))(context.defaultCurrency.apply)
end MoneyBigDecimalInstances
object MoneyBigDecimalInstances extends MoneyBigDecimalInstances
