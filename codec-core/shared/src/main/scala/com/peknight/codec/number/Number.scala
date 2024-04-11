package com.peknight.codec.number

import cats.Eq

/**
 * A number with optimization by cases.
 */
sealed trait Number extends Serializable derives CanEqual:
  private[codec] def toBiggerDecimal: BiggerDecimal
  /**
   * Return this number as a [[scala.math.BigDecimal]].
   */
  def toBigDecimal: Option[BigDecimal]
  /**
   * Return this number as a [[scala.math.BigInt]] if it's a sufficiently small whole number.
   */
  def toBigInt: Option[BigInt]

  /**
   * Return this number as a [[scala.Long]] if it's a valid [[scala.Long]].
   */
  def toLong: Option[Long]

  /**
   * Convert this number to its best [[scala.Double]] approximation.
   *
   * Anything over `Double.MaxValue` will be rounded to `Double.PositiveInfinity` and anything below
   * `Double.MinValue` is rounded to `Double.NegativeInfinity`.
   */
  def toDouble: Double

  /**
   * Convert this number to its best [[scala.Float]] approximation.
   *
   * Anything over `Float.MaxValue` will be rounded to `Float.PositiveInfinity` and anything below
   * `Float.MinValue` is rounded to `Float.NegativeInfinity`.
   */
  def toFloat: Float

  /**
   * Return this number as a [[scala.Byte]] if it's a valid [[scala.Byte]].
   */
  final def toByte: Option[Byte] = toLong match {
    case Some(n) =>
      val asByte: Byte = n.toByte
      if (n == asByte) Some(asByte) else None
    case None => None
  }

  /**
   * Return this number as a [[scala.Short]] if it's a valid [[scala.Short]].
   */
  final def toShort: Option[Short] = toLong match {
    case Some(n) =>
      val asShort: Short = n.toShort
      if (n == asShort) Some(asShort) else None
    case None => None
  }

  /**
   * Return this number as an [[scala.Int]] if it's a valid [[scala.Int]].
   */
  final def toInt: Option[Int] = toLong match {
    case Some(n) =>
      val asInt: Int = n.toInt
      if (n == asInt) Some(asInt) else None
    case None => None
  }

  /**
   * Universal equality derived from our type-safe equality.
   */
  override final def equals(that: Any): Boolean = that match {
    case that: Number => Number.eqNumber.eqv(this, that)
    case _ => false
  }

  /**
   * Hashing that is consistent with our universal equality.
   */
  override final def hashCode: Int = toBiggerDecimal.hashCode
end Number
object Number:
  private[codec] final case class BiggerDecimalNumber(value: BiggerDecimal) extends Number:
    private[codec] def toBiggerDecimal: BiggerDecimal = value
    def toBigDecimal: Option[BigDecimal] = toBiggerDecimal.toBigDecimal
    def toBigInt: Option[BigInt] = toBiggerDecimal.toBigInt
    def toDouble: Double = toBiggerDecimal.toDouble
    def toFloat: Float = toBiggerDecimal.toFloat
    def toLong: Option[Long] = toBiggerDecimal.toLong
    override def toString: String = value.toString
  end BiggerDecimalNumber

  /**
   * Represent a valid number as a `scala.math.BigDecimal`.
   */
  private[codec] final case class BigDecimalNumber(value: BigDecimal) extends Number:
    private[codec] def toBiggerDecimal: BiggerDecimal = BiggerDecimal.fromBigDecimal(value)
    def toBigDecimal: Option[BigDecimal] = Some(value)
    def toBigInt: Option[BigInt] = toBiggerDecimal.toBigInt
    def toDouble: Double = value.doubleValue
    def toFloat: Float = value.floatValue
    def toLong: Option[Long] = toBiggerDecimal.toLong
    override def toString: String = value.toString
  end BigDecimalNumber

  /**
   * Represent a valid number as a [[scala.Long]].
   */
  private[codec] final case class LongNumber(value: Long) extends Number:
    private[codec] def toBiggerDecimal: BiggerDecimal = BiggerDecimal.fromLong(value)
    def toBigDecimal: Option[BigDecimal] = Some(BigDecimal(value))
    def toBigInt: Option[BigInt] = Some(BigInt(value))
    def toDouble: Double = value.toDouble
    def toFloat: Float = value.toFloat
    def toLong: Option[Long] = Some(value)
    override def toString: String = s"$value"
  end LongNumber

  /**
   * Represent a valid number as a [[scala.Double]].
   */
  private[codec] final case class DoubleNumber(value: Double) extends Number:
    private[codec] def toBiggerDecimal: BiggerDecimal = BiggerDecimal.fromDoubleUnsafe(value)
    private[this] def toScalaBigDecimal: BigDecimal = BigDecimal(value)
    def toBigDecimal: Option[BigDecimal] = Some(toScalaBigDecimal)
    def toBigInt: Option[BigInt] =
      val asBigDecimal = toScalaBigDecimal
      if bigDecimalIsWhole(asBigDecimal) then Some(asBigDecimal.toBigInt) else None
    def toDouble: Double = value
    def toFloat: Float = value.toFloat
    def toLong: Option[Long] =
      val asBigDecimal = toScalaBigDecimal
      if bigDecimalIsValidLong(asBigDecimal) then Some(asBigDecimal.longValue) else None

    override def toString: String = s"$value"
  end DoubleNumber

  /**
   * Represent a valid number as a [[scala.Float]].
   */
  private[codec] final case class FloatNumber(value: Float) extends Number:
    private[codec] def toBiggerDecimal: BiggerDecimal = BiggerDecimal.fromFloat(value)
    private[this] def toScalaBigDecimal: BigDecimal = BigDecimal(s"$value")
    def toBigDecimal: Option[BigDecimal] = Some(toScalaBigDecimal)
    def toBigInt: Option[BigInt] =
      val asBigDecimal = toScalaBigDecimal
      if bigDecimalIsWhole(asBigDecimal) then Some(asBigDecimal.toBigInt) else None

    // Don't use `value.toFloat` due to floating point errors.
    def toDouble: Double = toScalaBigDecimal.doubleValue
    def toFloat: Float = value
    def toLong: Option[Long] =
      val asBigDecimal = toScalaBigDecimal
      if bigDecimalIsValidLong(asBigDecimal) then Some(asBigDecimal.longValue) else None
    override def toString: String = s"$value"
  end FloatNumber

  def fromString(value: String): Option[Number] =
    BiggerDecimal.parseBiggerDecimal(value) match
      case Right(Some(v)) => Some(BiggerDecimalNumber(v))
      case _ => None

  def fromStringUnsafe(value: String): Number =
    BiggerDecimalNumber(BiggerDecimal.parseBiggerDecimalUnsafe(value))

  private[this] val bigDecimalMinLong: BigDecimal = BigDecimal(Long.MinValue)
  private[this] val bigDecimalMaxLong: BigDecimal = BigDecimal(Long.MaxValue)

  def fromBiggerDecimal(value: BiggerDecimal): Number = BiggerDecimalNumber(value)
  def fromBigDecimal(value: BigDecimal): Number = BigDecimalNumber(value)
  def fromLong(value: Long): Number = LongNumber(value)
  def fromDouble(value: Double): Number = DoubleNumber(value)
  def fromFloat(value: Float): Number = FloatNumber(value)
  def fromInt(value: Int): Number = LongNumber(value.toLong)
  def fromByte(value: Byte): Number = LongNumber(value.toLong)
  def fromShort(value: Short): Number = LongNumber(value.toLong)
  def fromBigInt(value: BigInt): Number = BiggerDecimalNumber(BiggerDecimal.fromBigInt(value))

  private[codec] def bigDecimalIsWhole(value: BigDecimal): Boolean =
    value.signum == 0 || value.scale <= 0 || value.underlying().stripTrailingZeros().scale() <= 0

  private[codec] def bigDecimalIsValidLong(value: BigDecimal): Boolean =
    bigDecimalIsWhole(value) && value >= bigDecimalMinLong && value <= bigDecimalMaxLong

  given eqNumber: Eq[Number] with
    def eqv(x: Number, y: Number): Boolean = (x, y) match
      case (LongNumber(x), LongNumber(y)) => x == y
      case (DoubleNumber(x), DoubleNumber(y)) => x == y
      case (FloatNumber(x), FloatNumber(y)) => x == y
      case (BigDecimalNumber(x), BigDecimalNumber(y)) => x == y
      case (a, b) => a.toBiggerDecimal == b.toBiggerDecimal
  end eqNumber

end Number
