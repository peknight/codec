package com.peknight.codec.number

import cats.Eq

import scala.util.Try

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

  private[codec] def appendToStringBuilder(builder: StringBuilder): Unit
end Number
object Number:
  private[codec] sealed abstract class BiggerDecimalNumber(input: String) extends Number:
    final def toBigInt: Option[BigInt] = toBiggerDecimal.toBigInt
    final def toBigDecimal: Option[BigDecimal] = toBiggerDecimal.toBigDecimal.map { value =>
      if value == BiggerDecimal.ZeroDecimal then value
      else Try(BigDecimal(input)).getOrElse(value)
    }
    final def toLong: Option[Long] = toBiggerDecimal.toLong
    override final def toString: String = input
    private[codec] final def appendToStringBuilder(builder: StringBuilder): Unit = builder.append(input)
  end BiggerDecimalNumber

  /**
   * Represent a valid number as a `String`.
   */
  private[codec] final case class DecimalNum(input: String) extends BiggerDecimalNumber(input):
    private[codec] lazy val toBiggerDecimal: BiggerDecimal = BiggerDecimal.parseBiggerDecimalUnsafe(input)
    def toDouble: Double = input.toDouble
    def toFloat: Float = input.toFloat
  end DecimalNum

  private[codec] final case class BiggerDecimalNum(value: BiggerDecimal, input: String)
    extends BiggerDecimalNumber(input):
    private[codec] def toBiggerDecimal: BiggerDecimal = value
    def toDouble: Double = toBiggerDecimal.toDouble
    def toFloat: Float = toBiggerDecimal.toFloat
  end BiggerDecimalNum

  /**
   * Represent a valid number as a `scala.math.BigDecimal`.
   */
  private[codec] final case class BigDecimalNum(value: BigDecimal) extends Number:
    private[codec] def toBiggerDecimal: BiggerDecimal = BiggerDecimal.fromBigDecimal(value)
    def toBigDecimal: Option[BigDecimal] = Some(value)
    def toBigInt: Option[BigInt] = toBiggerDecimal.toBigInt
    def toDouble: Double = value.doubleValue
    def toFloat: Float = value.floatValue
    def toLong: Option[Long] = toBiggerDecimal.toLong
    override def toString: String = value.toString
    private[codec] def appendToStringBuilder(builder: StringBuilder): Unit = builder.append(value.toString)
  end BigDecimalNum

  /**
   * Represent a valid number as a [[scala.Long]].
   */
  private[codec] final case class LongNum(value: Long) extends Number:
    private[codec] def toBiggerDecimal: BiggerDecimal = BiggerDecimal.fromLong(value)
    def toBigDecimal: Option[BigDecimal] = Some(BigDecimal(value))
    def toBigInt: Option[BigInt] = Some(BigInt(value))
    def toDouble: Double = value.toDouble
    def toFloat: Float = value.toFloat
    def toLong: Option[Long] = Some(value)
    override def toString: String = s"$value"
    private[codec] def appendToStringBuilder(builder: StringBuilder): Unit = builder.append(value)
  end LongNum

  /**
   * Represent a valid number as a [[scala.Double]].
   */
  private[codec] final case class DoubleNum(value: Double) extends Number:
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
    private[codec] def appendToStringBuilder(builder: StringBuilder): Unit = builder.append(value)
  end DoubleNum

  /**
   * Represent a valid number as a [[scala.Float]].
   */
  private[codec] final case class FloatNum(value: Float) extends Number:
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
    private[codec] def appendToStringBuilder(builder: StringBuilder): Unit = builder.append(value)
  end FloatNum


  /**
   * Return a `Number` whose value is the valid number in `value`.
   *
   * @note This value is ''not'' verified to be a valid string. It is assumed that `value` is a
   * valid number, according to the specification. If the value is invalid the behavior is
   * undefined. This operation is provided for use in situations where the validity of the input has
   * already been verified.
   */
  def fromDecimalStringUnsafe(value: String): Number = DecimalNum(value)

  /**
   * Return a `Number` whose value is the valid integral number in `value`.
   *
   * @note This value is ''not'' verified to be a valid string. It is assumed that `value` is a
   * valid number, according to the specification. If the value is invalid the behavior is
   * undefined. This operation is provided for use in situations where the validity of the input has
   * already been verified.
   */
  def fromIntegralStringUnsafe(value: String): Number =
    if !BiggerDecimal.integralIsValidLong(value) then DecimalNum(value)
    else
      val longValue = value.toLong
      if value.head == '-' && longValue == 0L then DecimalNum(value) else LongNum(longValue)

  def fromString(value: String): Option[Number] =
    BiggerDecimal.parseBiggerDecimal(value) match
      case Right(Some(v)) => Some(BiggerDecimalNum(v, value))
      case _ => None

  private[this] val bigDecimalMinLong: BigDecimal = BigDecimal(Long.MinValue)
  private[this] val bigDecimalMaxLong: BigDecimal = BigDecimal(Long.MaxValue)

  def fromBiggerDecimal(value: BiggerDecimal, input: String): Number = BiggerDecimalNum(value, input)
  def fromBigDecimal(value: BigDecimal): Number = BigDecimalNum(value)
  def fromLong(value: Long): Number = LongNum(value)
  def fromDouble(value: Double): Number = DoubleNum(value)
  def fromFloat(value: Float): Number = FloatNum(value)
  def fromInt(value: Int): Number = LongNum(value.toLong)
  def fromByte(value: Byte): Number = LongNum(value.toLong)
  def fromShort(value: Short): Number = LongNum(value.toLong)
  def fromBigInt(value: BigInt): Number = BiggerDecimalNum(BiggerDecimal.fromBigInt(value), value.toString)

  private[codec] def bigDecimalIsWhole(value: BigDecimal): Boolean =
    value.signum == 0 || value.scale <= 0 || value.underlying().stripTrailingZeros().scale() <= 0

  private[codec] def bigDecimalIsValidLong(value: BigDecimal): Boolean =
    bigDecimalIsWhole(value) && value >= bigDecimalMinLong && value <= bigDecimalMaxLong

  given eqNumber: Eq[Number] with
    def eqv(x: Number, y: Number): Boolean = (x, y) match
      case (LongNum(x), LongNum(y)) => x == y
      case (DoubleNum(x), DoubleNum(y)) => x == y
      case (FloatNum(x), FloatNum(y)) => x == y
      case (BigDecimalNum(x), BigDecimalNum(y)) => x == y
      case (a, b) => a.toBiggerDecimal == b.toBiggerDecimal
  end eqNumber

end Number
