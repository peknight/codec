package com.peknight.codec.number

/**
 * Represents a large decimal number.
 *
 * In theory `BigDecimal` can represent a very large range of valid numbers (in most cases if a
 * number string can fit in memory, it's possible to construct an exact `BigDecimal`
 * representation), but in practice this becomes intractable for many small numbers (e.g.
 * "1e2147483648" cannot be directly parsed as a `BigDecimal`).
 *
 * This type makes it possible to represent much, much larger numbers efficiently (although it
 * doesn't support many operations on these values). It also makes it possible to distinguish
 * between positive and negative zeros (unlike `BigDecimal`), which may be useful in some
 * applications.
 */
sealed trait BiggerDecimal extends Serializable:
  def isWhole: Boolean
  def isNegativeZero: Boolean

  /**
   * The sign of this value.
   *
   * Returns -1 if it is less than 0, +1 if it is greater than 0, and 0 if it is
   * equal to 0. Note that this follows the behavior of [[scala.Double]] for
   * negative zero (returning 0).
   */
  def signum: Int

  /**
   * Convert to a `BigDecimal` if the `scale` is within the range of [[scala.Int]].
   */
  def toBigDecimal: Option[BigDecimal]

  /**
   * Convert to a `BigInt` if this is a sufficiently small whole number.
   */
  def toBigIntWithMaxDigits(maxDigits: BigInt): Option[BigInt]

  /**
   * Convert to a `BigInt` if this is a sufficiently small whole number.
   *
   * The maximum number of digits is somewhat arbitrarily set at 2^18 digits, since larger values
   * may require excessive processing power. Larger values may be converted to `BigInteger` with
   * [[toBigIntWithMaxDigits]] or via [[toBigDecimal]].
   */
  final def toBigInt: Option[BigInt] = toBigIntWithMaxDigits(BiggerDecimal.MaxBigIntegerDigits)

  /**
   * Convert to the nearest [[scala.Double]].
   */
  def toDouble: Double

  /**
   * Convert to the nearest [[scala.Float]].
   */
  def toFloat: Float

  /**
   * Convert to a [[scala.Long]] if this is a valid `Long` value.
   */
  def toLong: Option[Long]

  private[codec] def appendToStringBuilder(builder: StringBuilder): Unit
end BiggerDecimal
object BiggerDecimal:
  private[number] val MaxBigIntegerDigits: BigInt = BigInt(1L << 18)

  private[number] val MaxInt: BigInt = BigInt(Int.MaxValue)
  private[number] val MinInt: BigInt = BigInt(Int.MinValue)
  private[number] val MaxLong: BigDecimal = BigDecimal(Long.MaxValue)
  private[number] val MinLong: BigDecimal = BigDecimal(Long.MinValue)

  /**
   * Represents numbers as an unscaled value and a scale.
   *
   * This representation is the same as that used by `java.math.BigDecimal`, with two differences.
   * First, the scale is a `java.math.BigInteger`, not a [[scala.Int]], and the unscaled value will
   * never be an exact multiple of ten (in order to facilitate comparison).
   */
  private[number] case class SigAndExp(unscaled: BigInt, scale: BigInt) extends BiggerDecimal:
    def isWhole: Boolean = scale.signum < 1
    def isNegativeZero: Boolean = false
    def signum: Int = unscaled.signum
    def toBigDecimal: Option[BigDecimal] =
      if MinInt <= scale && scale <= MaxInt then Some(BigDecimal(unscaled, scale.intValue))
      else None
    def toBigIntWithMaxDigits(maxDigits: BigInt): Option[BigInt] =
      if !isWhole then None
      else
        val digits = BigInt(unscaled.abs.toString.length.toLong) - scale
        if digits > maxDigits then None
        else Some(BigDecimal(unscaled, scale.intValue).toBigInt)

    def toDouble: Double =
      if MinInt <= scale && scale <= MaxInt then BigDecimal(unscaled, scale.intValue).doubleValue
      else (if scale.signum == 1 then 0.0 else Double.PositiveInfinity) * unscaled.signum

    def toFloat: Float =
      if MinInt <= scale && scale <= MaxInt then BigDecimal(unscaled, scale.intValue).floatValue
      else (if scale.signum == 1 then 0.0f else Float.PositiveInfinity) * unscaled.signum

    def toLong: Option[Long] =
      if !isWhole then None
      else
        toBigInt match
          case Some(i) =>
            val asLong = i.longValue
            if BigInt(asLong) == i then Some(asLong) else None
          case None => None

    override def equals(that: Any): Boolean = that match
      case other: SigAndExp => unscaled == other.unscaled && scale == other.scale
      case _ => false

    override def hashCode: Int = scale.hashCode + unscaled.hashCode

    override def toString: String =
      if scale == BigInt(0) then unscaled.toString
      else s"${unscaled}e${scale.negate}"

    override private[codec] def appendToStringBuilder(builder: StringBuilder): Unit =
      builder.append(unscaled)
      if scale != BigInt(0) then builder.append('e').append(scale.negate) else ()
    end appendToStringBuilder
  end SigAndExp
end BiggerDecimal
