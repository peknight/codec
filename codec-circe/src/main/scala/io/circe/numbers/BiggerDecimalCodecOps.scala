package io.circe.numbers

object BiggerDecimalCodecOps:
  //noinspection TypeCheckCanBeMatch
  def migrate(biggerDecimal: BiggerDecimal): com.peknight.codec.number.BiggerDecimal =
    given CanEqual[BiggerDecimal, BiggerDecimal] = CanEqual.derived
    if biggerDecimal == BiggerDecimal.NegativeZero then com.peknight.codec.number.BiggerDecimal.negativeZero
    else
      val str = biggerDecimal.toString
      if str == "0" then com.peknight.codec.number.BiggerDecimal.unsignedZero
      else if biggerDecimal.isInstanceOf[SigAndExp] then
        val exp = biggerDecimal.asInstanceOf[SigAndExp]
        com.peknight.codec.number.BiggerDecimal(BigInt(exp.unscaled), BigInt(exp.scale))
      else com.peknight.codec.number.BiggerDecimal.parseBiggerDecimalUnsafe(str)
end BiggerDecimalCodecOps
