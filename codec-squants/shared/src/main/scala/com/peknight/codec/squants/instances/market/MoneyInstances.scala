package com.peknight.codec.squants.instances.market

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.Encoder
import com.peknight.codec.number.Number
import squants.market.{Money, MoneyContext}

trait MoneyInstances extends MoneyInstances1:
  given numberEncodeMoney[F[_]](using applicative: Applicative[F], context: MoneyContext): Encoder[F, Number, Money] =
    Encoder.instance[F, Number, Money](money => Number.fromBigDecimal(money.to(context.defaultCurrency)).pure[F])
end MoneyInstances
object MoneyInstances extends MoneyInstances
