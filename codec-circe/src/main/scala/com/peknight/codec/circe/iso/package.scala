package com.peknight.codec.circe

import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.{Applicative, Id}
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.cursor.{Cursor, CursorOp}
import com.peknight.codec.derivation.{SumDecoder, SumEncoder}
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.error.DecodingFailure.{Common, Errors}
import com.peknight.codec.number.{BiggerDecimal, Number}
import com.peknight.codec.{Codec, Decoder, Encoder}
import com.peknight.error.std.WrongType
import com.peknight.error.{Error, Lift}
import com.peknight.generic.migration.Isomorphism
import io.circe.DecodingFailure.Reason.WrongTypeExpectation
import io.circe.cursor.CursorOps
import io.circe.derivation.{ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder, SumOrProductOps}
import io.circe.numbers.{BiggerDecimalCodecOps, BiggerDecimalOps}
import io.circe.{ACursor, HCursor, Json, JsonNumber, JsonNumberCodecOps, JsonNumberOps, JsonObject, numbers}

package object iso:

  given cursorOpIsomorphism[F[_]: Applicative]: Isomorphism[F, CursorOp, io.circe.CursorOp] with
    given CanEqual[io.circe.CursorOp, io.circe.CursorOp] = CanEqual.derived
    def to(a: CursorOp): F[io.circe.CursorOp] = a match
      case CursorOp.MoveLeft => io.circe.CursorOp.MoveLeft.pure
      case CursorOp.MoveRight => io.circe.CursorOp.MoveRight.pure
      case CursorOp.MoveUp => io.circe.CursorOp.MoveUp.pure
      case CursorOp.Field(k) => io.circe.CursorOp.Field(k).pure
      case CursorOp.DownField(k) => io.circe.CursorOp.DownField(k).pure
      case CursorOp.DownArray => io.circe.CursorOp.DownArray.pure
      case CursorOp.DownN(n) => io.circe.CursorOp.DownN(n).pure
      case CursorOp.DeleteGoParent => io.circe.CursorOp.DeleteGoParent.pure
    def from(b: io.circe.CursorOp): F[CursorOp] = b match
      case io.circe.CursorOp.MoveLeft => CursorOp.MoveLeft.pure
      case io.circe.CursorOp.MoveRight => CursorOp.MoveRight.pure
      case io.circe.CursorOp.MoveUp => CursorOp.MoveUp.pure
      case io.circe.CursorOp.Field(k) => CursorOp.Field(k).pure
      case io.circe.CursorOp.DownField(k) => CursorOp.DownField(k).pure
      case io.circe.CursorOp.DownArray => CursorOp.DownArray.pure
      case io.circe.CursorOp.DownN(n) => CursorOp.DownN(n).pure
      case io.circe.CursorOp.DeleteGoParent => CursorOp.DeleteGoParent.pure
  end cursorOpIsomorphism

  given successCursorIsomorphism[F[_]: Applicative]: Isomorphism[F, SuccessCursor[Json], HCursor] with
    def to(a: SuccessCursor[Json]): F[HCursor] = a match
      case Cursor.TCursor(value, lastCursor, lastOp) =>
        CursorOps.topCursor(value, lastCursor.map(successCursorIsomorphism[Id].to).orNull,
          lastOp.map(cursorOpIsomorphism[Id].to).orNull).pure
      case Cursor.OCursor(obj, keyValue, parent, changed, _, lastCursor, lastOp) =>
        CursorOps.objectCursor(obj.asInstanceOf[JsonObject], keyValue, successCursorIsomorphism[Id].to(parent), changed,
          lastCursor.map(successCursorIsomorphism[Id].to).orNull, lastOp.map(cursorOpIsomorphism[Id].to).orNull).pure
      case Cursor.ACursor(array, indexValue, parent, changed, _, lastCursor, lastOp) =>
        CursorOps.arrayCursor(array, indexValue, successCursorIsomorphism[Id].to(parent), changed,
          lastCursor.map(successCursorIsomorphism[Id].to).orNull, lastOp.map(cursorOpIsomorphism[Id].to).orNull).pure
    def from(b: HCursor): F[SuccessCursor[Json]] = Cursor.TCursor(b.value, None, None).pure
  end successCursorIsomorphism
  given failedCursorIsomorphism[F[_]: Applicative]: Isomorphism[F, FailedCursor[Json], io.circe.FailedCursor] with
    def to(a: FailedCursor[Json]): F[io.circe.FailedCursor] =
      io.circe.FailedCursor(
        a.lastCursor.map(successCursorIsomorphism[Id].to).orNull,
        a.lastOp.map(cursorOpIsomorphism[Id].to).orNull
      ).pure
    def from(b: io.circe.FailedCursor): F[FailedCursor[Json]] = Cursor.FCursor(None, None).pure
  end failedCursorIsomorphism
  given cursorIsomorphism[F[_]: Applicative]: Isomorphism[F, Cursor[Json], ACursor] with
    def to(a: Cursor[Json]): F[ACursor] = a match
      case cursor: SuccessCursor[Json] => successCursorIsomorphism[F].to(cursor).asInstanceOf[F[ACursor]]
      case cursor: FailedCursor[Json] => failedCursorIsomorphism[F].to(cursor).asInstanceOf[F[ACursor]]
    def from(b: ACursor): F[Cursor[Json]] = b match
      case cursor: HCursor => successCursorIsomorphism[F].from(cursor).asInstanceOf[F[Cursor[Json]]]
      case cursor: io.circe.FailedCursor => failedCursorIsomorphism[F].from(cursor).asInstanceOf[F[Cursor[Json]]]
  end cursorIsomorphism
  given decodingFailureIsomorphism[F[_]: Applicative]: Isomorphism[F, DecodingFailure, io.circe.DecodingFailure] with
    def to(failure: DecodingFailure): F[io.circe.DecodingFailure] = failure match
      case Common(e: WrongType, _, _, Some((value: Json, _)), _, _, Some(history)) =>
        io.circe.DecodingFailure(WrongTypeExpectation(e.expectedType, value), history.map(cursorOpIsomorphism[Id].to))
          .pure
      case Common(e: Error, _, _, _, _, _, Some(history)) =>
        io.circe.DecodingFailure(e.message, history.map(cursorOpIsomorphism[Id].to)).pure
      case Common(e, _, _, _, _, _, Some(history)) =>
        io.circe.DecodingFailure(Error.pureMessage(e), history.map(cursorOpIsomorphism[Id].to)).pure
      case e: Lift[?] if e.error.isInstanceOf[io.circe.DecodingFailure] =>
        e.error.asInstanceOf[io.circe.DecodingFailure].pure
      case _ => io.circe.DecodingFailure(failure.message, List.empty[io.circe.CursorOp]).pure
    def from(b: io.circe.DecodingFailure): F[DecodingFailure] = DecodingFailure(b).pure
  end decodingFailureIsomorphism
  given decodingFailuresIsomorphism[F[_]: Applicative]
  : Isomorphism[F, DecodingFailure, NonEmptyList[io.circe.DecodingFailure]] with
    def to(failure: DecodingFailure): F[NonEmptyList[io.circe.DecodingFailure]] = failure match
      case Errors(errors) => errors.map(decodingFailureIsomorphism[Id].to).pure[F]
      case error => NonEmptyList.one(decodingFailureIsomorphism[Id].to(error)).pure[F]
    def from(b: NonEmptyList[io.circe.DecodingFailure]): F[DecodingFailure] = DecodingFailure(b).pure
  end decodingFailuresIsomorphism
  given encoderIsomorphism[F[_]: Applicative, A]: Isomorphism[F, Encoder[Id, Json, A], io.circe.Encoder[A]] with
    def to(a: Encoder[Id, Json, A]): F[io.circe.Encoder[A]] = com.peknight.codec.circe.Encoder(a).pure
    def from(b: io.circe.Encoder[A]): F[Encoder[Id, Json, A]] = b match
      case e: com.peknight.codec.circe.Encoder[A] => e.encoder.pure
      case e: ConfiguredEncoder[A] if SumOrProductOps.isSum(e) =>
        val encoder = new SumEncoder[Id, Json, A]:
          def encode(a: A): Json = b(a)
        encoder.pure
      case e => Encoder.instance[Id, Json, A](b.apply).pure
  end encoderIsomorphism

  private def migrateDecoder[T](b: io.circe.Decoder[T]): Decoder[Id, Cursor[Json], T] = b match
    case d: com.peknight.codec.circe.Decoder[T] => d.decoder
    case d: ConfiguredDecoder[T] if SumOrProductOps.isSum(d) =>
      new SumDecoder[Id, Cursor[Json], T]:
        def decoders: Map[String, Decoder[Id, Cursor[Json], ?]] =
          d.constructorNames.zip(d.elemDecoders).map((key, dd) => (key, migrateDecoder(dd))).toMap
        def decode(cursor: Cursor[Json]): Either[DecodingFailure, T] =
          d.tryDecodeAccumulating(cursorIsomorphism[Id].to(cursor))
            .toEither.left.map(decodingFailuresIsomorphism[Id].from)
    case d =>
      new Decoder[Id, Cursor[Json], T]:
        def decode(cursor: Cursor[Json]): Either[DecodingFailure, T] =
          d.tryDecodeAccumulating(cursorIsomorphism[Id].to(cursor))
            .toEither.left.map(decodingFailuresIsomorphism[Id].from)
  end migrateDecoder

  given decoderIsomorphism[F[_]: Applicative, A]: Isomorphism[F, Decoder[Id, Cursor[Json], A], io.circe.Decoder[A]] with
    def to(a: Decoder[Id, Cursor[Json], A]): F[io.circe.Decoder[A]] = com.peknight.codec.circe.Decoder(a).pure
    def from(b: io.circe.Decoder[A]): F[Decoder[Id, Cursor[Json], A]] = migrateDecoder(b).pure
  end decoderIsomorphism


  given codecIsomorphism[F[_]: Applicative, A]: Isomorphism[F, Codec[Id, Json, Cursor[Json], A], io.circe.Codec[A]] with
    def to(a: Codec[Id, Json, Cursor[Json], A]): F[io.circe.Codec[A]] = com.peknight.codec.circe.Codec(a).pure
    def from(b: io.circe.Codec[A]): F[Codec[Id, Json, Cursor[Json], A]] = b match
      case c: com.peknight.codec.circe.Codec[A] => c.codec.pure
      case c: ConfiguredCodec[A] if SumOrProductOps.isSum(c) =>
        val codec = new Codec[Id, Json, Cursor[Json], A] with SumEncoder[Id, Json, A]
          with SumDecoder[Id, Cursor[Json], A]:
          def decoders: Map[String, Decoder[Id, Cursor[Json], ?]] =
            c.constructorNames.zip(c.elemDecoders).map((key, dd) => (key, migrateDecoder(dd))).toMap
          def encode(a: A): Json = c(a)
          def decode(cursor: Cursor[Json]): Either[DecodingFailure, A] =
            c.tryDecodeAccumulating(cursorIsomorphism[Id].to(cursor))
              .toEither.left.map(decodingFailuresIsomorphism[Id].from)
        codec.pure
      case c =>
        val codec = new Codec[Id, Json, Cursor[Json], A]:
          def encode(a: A): Json = c(a)
          def decode(cursor: Cursor[Json]): Either[DecodingFailure, A] =
            c.tryDecodeAccumulating(cursorIsomorphism[Id].to(cursor))
              .toEither.left.map(decodingFailuresIsomorphism[Id].from)
        codec.pure
  end codecIsomorphism

  given biggerDecimalIsomorphism[F[_]: Applicative]: Isomorphism[F, BiggerDecimal, io.circe.numbers.BiggerDecimal] with
    def to(a: BiggerDecimal): F[io.circe.numbers.BiggerDecimal] = a match
      case BiggerDecimal.SigAndExp(unscaled, scale) => BiggerDecimalOps.sigAndExp(unscaled, scale).pure
      case BiggerDecimal.UnsignedZero => io.circe.numbers.BiggerDecimal.fromLong(0L).pure
      case BiggerDecimal.NegativeZero => io.circe.numbers.BiggerDecimal.NegativeZero.pure

    def from(b: io.circe.numbers.BiggerDecimal): F[BiggerDecimal] = BiggerDecimalCodecOps.migrate(b).pure
  end biggerDecimalIsomorphism

  given numberIsomorphism[F[_]: Applicative]: Isomorphism[F, Number, JsonNumber] with
    def to(a: Number): F[JsonNumber] = a match
      case Number.BiggerDecimalNumber(value) =>
        JsonNumberOps.fromBiggerDecimal(biggerDecimalIsomorphism[Id].to(value), value.toString()).pure
      case Number.BigDecimalNumber(value) => JsonNumberOps.fromBigDecimal(value).pure
      case Number.LongNumber(value) => JsonNumberOps.fromLong(value).pure
      case Number.DoubleNumber(value) => JsonNumberOps.fromDouble(value).pure
      case Number.FloatNumber(value) => JsonNumberOps.fromFloat(value).pure

    def from(b: JsonNumber): F[Number] = JsonNumberCodecOps.migrate(b).pure
  end numberIsomorphism

  def encoder[A](using encoder: Encoder[Id, Json, A]): io.circe.Encoder[A] = encoderIsomorphism[Id, A].to(encoder)
  def decoder[A](using decoder: Decoder[Id, Cursor[Json], A]): io.circe.Decoder[A] =
    decoderIsomorphism[Id, A].to(decoder)
  def codec[A](using encoder: Encoder[Id, Json, A], decoder: Decoder[Id, Cursor[Json], A])
  : io.circe.Codec[A] =
    codecIsomorphism[Id, A].to(Codec[Id, Json, Cursor[Json], A])
end iso
