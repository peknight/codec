package com.peknight.codec.number

import cats.parse.Numbers.digits0
import cats.parse.Parser
import cats.syntax.either.*
import cats.syntax.option.*
import com.peknight.error.parse.ParsingFailure

import java.math.BigInteger
import scala.annotation.tailrec

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
sealed trait BiggerDecimal extends Serializable derives CanEqual:
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

end BiggerDecimal
object BiggerDecimal:
  private[number] val MaxBigIntegerDigits: BigInt = BigInt(1L << 18)

  private[number] val MaxInt: BigInt = BigInt(Int.MaxValue)
  private[number] val MinInt: BigInt = BigInt(Int.MinValue)
  private[number] val MaxLong: BigDecimal = BigDecimal(Long.MaxValue)
  private[number] val MinLong: BigDecimal = BigDecimal(Long.MinValue)
  private[this] val MaxLongString = "9223372036854775807"
  private[this] val MinLongString = "-9223372036854775808"
  private[number] val ZeroInt: BigInt = BigInt(0)
  private[number] val ZeroDecimal: BigDecimal = BigDecimal(0)

  /**
   * Represents numbers as an unscaled value and a scale.
   *
   * This representation is the same as that used by `java.math.BigDecimal`, with two differences.
   * First, the scale is a `java.math.BigInteger`, not a [[scala.Int]], and the unscaled value will
   * never be an exact multiple of ten (in order to facilitate comparison).
   */
  private[codec] case class SigAndExp(unscaled: BigInt, scale: BigInt) extends BiggerDecimal:
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
      if scale == ZeroInt then unscaled.toString
      else s"${unscaled}e${scale.underlying().negate()}"
  end SigAndExp

  private[codec] sealed trait Zero extends BiggerDecimal:
    final def isWhole: Boolean = true
    final def signum: Int = 0
    final val toBigDecimal: Option[BigDecimal] = Some(ZeroDecimal)
    final def toBigIntWithMaxDigits(maxDigits: BigInt): Option[BigInt] = Some(ZeroInt)
    final val toLong: Option[Long] = Some(0L)
  end Zero

  private[codec] object UnsignedZero extends Zero:
    def isNegativeZero: Boolean = false
    def toDouble: Double = 0.0
    def toFloat: Float = 0.0f
    override def equals(that: Any): Boolean = that match
      case other: Zero => !other.isNegativeZero
      case _ => false
    override def hashCode: Int = 0.0.hashCode
    override def toString: String = "0"
  end UnsignedZero
  private[codec] object NegativeZero extends Zero:
    def isNegativeZero: Boolean = true
    def toDouble: Double = -0.0
    def toFloat: Float = -0.0f
    override def equals(that: Any): Boolean = that match
      case other: Zero => other.isNegativeZero
      case _ => false
    override def hashCode: Int = -0.0.hashCode
    override def toString: String = "-0"
  end NegativeZero

  val unsignedZero: BiggerDecimal = UnsignedZero
  val negativeZero: BiggerDecimal = NegativeZero
  def apply(unscaled: BigInt, scaled: BigInt): BiggerDecimal =
    if unscaled == ZeroInt then UnsignedZero else SigAndExp(unscaled, scaled)

  private[this] def fromUnscaledAndScale(unscaled: BigInt, scale: Long): BiggerDecimal =
    @tailrec def go(current: BigInteger, depth: Long, divAndRem: Array[BigInteger])
    : (BigInteger, Long, Array[BigInteger]) =
      if divAndRem(1) == BigInteger.ZERO then
        val nextCurrent = divAndRem(0)
        go(nextCurrent, depth - 1L, nextCurrent.divideAndRemainder(BigInteger.TEN))
      else (current, depth, divAndRem)
    if unscaled == ZeroInt then UnsignedZero
    else
      val unscaledValue = unscaled.underlying()
      val (current, depth, _) = go(unscaledValue, scale, unscaledValue.divideAndRemainder(BigInteger.TEN))
      SigAndExp(BigInt(current), BigInt(depth))

  def fromBigInt(i: BigInt): BiggerDecimal = fromUnscaledAndScale(i, 0L)
  def fromBigDecimal(d: BigDecimal): BiggerDecimal = fromUnscaledAndScale(d.underlying().unscaledValue(), d.scale.toLong)
  def fromLong(d: Long): BiggerDecimal = fromUnscaledAndScale(BigInt(d), 0L)

  /**
   * Convert a [[scala.Double]] into a [[BiggerDecimal]].
   *
   * @note This method assumes that the input is not `NaN` or infinite, and will throw a
   *       `NumberFormatException` if that assumption does not hold.
   */
  def fromDoubleUnsafe(d: Double): BiggerDecimal =
    if java.lang.Double.compare(d, -0.0) == 0 then NegativeZero else fromBigDecimal(BigDecimal(d))

  def fromFloat(f: Float): BiggerDecimal =
    if java.lang.Float.compare(f, -0.0f) == 0 then NegativeZero
    else fromBigDecimal(BigDecimal(java.lang.Float.toString(f)))

  /**
   * Is a string representing an integral value a valid [[scala.Long]]?
   *
   * Note that this method assumes that the input is a valid integral
   * number string (e.g. that it does have leading zeros).
   */
  def integralIsValidLong(s: String): Boolean =
    val bound = if s.charAt(0) == '-' then MinLongString else MaxLongString
    s.length < bound.length || (s.length == bound.length && s.compareTo(bound) <= 0)

  /**
   * Parse string into [[BiggerDecimal]].
   */
  def parseBiggerDecimal(input: String): Either[ParsingFailure, Option[BiggerDecimal]] =
    if input.isEmpty then none[BiggerDecimal].asRight[ParsingFailure]
    else
      val negativeParser = (Parser.char('-').as(true) | Parser.char('+').as(false)).?.map(_.getOrElse(false))
      val parser =
        for
          negativeFlag <- negativeParser
          integral <- digits0
          fractional <- (Parser.char('.') *> digits0).?.map(_.filter(_.nonEmpty))
          exponent <- (Parser.charIn("eE") *> (negativeParser ~ digits0)).?
        yield
          val negativeStr = if negativeFlag then "-" else ""
          val integralStr = if integral.nonEmpty then integral else "0"
          val fractionalStr = fractional.getOrElse("")
          val unsignedStr = s"$integralStr$fractionalStr"
          val zeros: Int = unsignedStr
            .zipWithIndex
            .findLast(_._1 != '0')
            .map(unsignedStr.length - 1 - _._2)
            .getOrElse(0)
          if unsignedStr.length == zeros then
            if negativeFlag then NegativeZero else UnsignedZero
          else
            val unscaledStr =
              if zeros == 0 then s"$negativeStr$unsignedStr"
              else s"$negativeStr${unsignedStr.substring(0, unsignedStr.length - zeros)}"
            val unscaled = BigInt(unscaledStr)
            if unscaled == ZeroInt then
              if negativeFlag then NegativeZero else UnsignedZero
            else
              val rescale = BigInt(fractionalStr.length - zeros)
              val scale =
                exponent match
                  case Some((expNeg, exp)) => rescale - (if expNeg then BigInt(s"-$exp") else BigInt(exp))
                  case None => rescale
              SigAndExp(unscaled, scale)
      parser.parseAll(input).left.map(ParsingFailure.apply).map(_.some)

  /**
   * Parse string into [[BiggerDecimal]]. throw exception on parsing failure.
   */
  def parseBiggerDecimalUnsafe(input: String): BiggerDecimal =
    parseBiggerDecimal(input) match
      case Left(error) =>
        throw error.to(NumberFormatException(s"For input string \"$input\""))
      case Right(None) =>
        throw Error(NumberFormatException(s"For input string \"$input\""))
      case Right(Some(value)) => value
end BiggerDecimal
