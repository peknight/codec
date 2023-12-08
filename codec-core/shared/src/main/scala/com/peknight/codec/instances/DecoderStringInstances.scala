package com.peknight.codec.instances

import cats.data.ValidatedNel
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.validated.*
import cats.{Applicative, Functor}
import com.peknight.codec.cursor.{Cursor, CursorDecoder, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, ParsingTypeError, WrongClassTag}
import com.peknight.codec.sum.StringType

import java.util.UUID
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait DecoderStringInstances:

  private[this] type Decoder[F[_], A] = com.peknight.codec.Decoder[F, String, DecodingFailure[String], A]

  private[this] def instance[F[_]: Functor, A](f: String => F[Either[DecodingFailure[String], A]]): Decoder[F, A] =
    com.peknight.codec.Decoder.instance[F, String, DecodingFailure[String], A](f)

  given decodeBoolean[F[_]: Applicative]: Decoder[F, Boolean] =
    instance[F, Boolean] { t =>
      toBooleanOption(t) match
        case Some(b) => b.asRight.pure
        case None => WrongClassTag[String, Boolean](t).asLeft.pure
    }

  private[this] def toBooleanOption(t: String): Option[Boolean] =
    if "true".equalsIgnoreCase(t) then Some(true)
    else if "false".equalsIgnoreCase(t) then Some(false)
    else if List("1", "t", "yes", "y").exists(t.equalsIgnoreCase) then Some(true)
    else if List("0", "f", "no", "n").exists(t.equalsIgnoreCase) then Some(false)
    else None

  given decodeChar[F[_]: Applicative]: Decoder[F, Char] =
    instance[F, Char](t => if t.length == 1 then t.head.asRight.pure else WrongClassTag[String, Char](t).asLeft.pure)

  private[this] def decodeNumber[F[_]: Applicative, A: ClassTag](f: BigDecimal => A): Decoder[F, A] =
    instance[F, A] { t =>
      Try(f(BigDecimal(t))) match
        case Success(value) => value.asRight.pure
        case Failure(e) => ParsingTypeError[String, A](t, e).asLeft.pure
    }

  given decodeFloat[F[_]: Applicative]: Decoder[F, Float] = decodeNumber[F, Float](_.toFloat)
  given decodeDouble[F[_]: Applicative]: Decoder[F, Double] = decodeNumber[F, Double](_.toDouble)
  given decodeByte[F[_]: Applicative]: Decoder[F, Byte] = decodeNumber[F, Byte](_.toByte)
  given decodeShort[F[_]: Applicative]: Decoder[F, Short] = decodeNumber[F, Short](_.toShort)
  given decodeInt[F[_]: Applicative]: Decoder[F, Int] = decodeNumber[F, Int](_.toInt)
  given decodeLong[F[_]: Applicative]: Decoder[F, Long] = decodeNumber[F, Long](_.toLong)
  given decodeBigInt[F[_]: Applicative]: Decoder[F, BigInt] = decodeNumber[F, BigInt](_.toBigInt)
  given decodeBigDecimal[F[_]: Applicative]: Decoder[F, BigDecimal] = decodeNumber[F, BigDecimal](identity)

  given decodeUUID[F[_]: Applicative]: Decoder[F, UUID] = instance[F, UUID] { t =>
    Try(UUID.fromString(t)) match
      case Success(value) => value.asRight.pure
      case Failure(e) => ???
  }

  given decodeString[F[_], S](using Applicative[F], StringType[S]): CursorDecoder[F, S, String] =
    CursorDecoder.instance[F, S, String] { t =>
      StringType[S].asString(t.value) match
        case Some(s) => s.asRight.pure
        case None => WrongClassTag[Cursor[S], String](t).asLeft.pure
    }

end DecoderStringInstances
