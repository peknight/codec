package com.peknight.codec.squants.instances

import cats.{Applicative, Show}
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
import squants.market.*

trait CurrencyInstances:
  private val currencies: List[Currency] = List(
    USD,
    ARS,
    AUD,
    BRL,
    CAD,
    CHF,
    CLP,
    CNY,
    CZK,
    DKK,
    EUR,
    GBP,
    HKD,
    INR,
    JPY,
    KRW,
    MXN,
    MYR,
    NOK,
    NZD,
    RUB,
    SEK,
    XAG,
    XAU,
    BTC,
    ETH,
    LTC,
    ZAR,
    NAD,
    TRY
  )

  given stringCodecCurrency[F[_]: Applicative]: Codec[F, String, String, Currency] =
    Codec.mapOption[F, String, String, Currency](_.name)(t => currencies.find(_.code.equalsIgnoreCase(t)))

  given codecCurrencyS[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], Currency] =
    Codec.codecS[F, S, Currency]
end CurrencyInstances
object CurrencyInstances extends CurrencyInstances


