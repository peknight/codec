package com.peknight.codec.circe

import cats.Id
import cats.data.ValidatedNel
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.cursor.id.{Codec, Decoder}
import com.peknight.codec.cursor.{Cursor, CursorOp}
import com.peknight.codec.derivation.{SumDecoder, SumEncoder}
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.error.DecodingFailure.Common
import com.peknight.codec.id.Encoder
import com.peknight.error.std.WrongType
import com.peknight.error.{Error, Lift}
import com.peknight.generic.migration.id.Isomorphism
import io.circe.DecodingFailure.Reason.WrongTypeExpectation
import io.circe.cursor.CursorOps
import io.circe.derivation.{ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder, SumOrProductOps}
import io.circe.{ACursor, HCursor, Json, JsonObject}

package object iso:

  given cursorOpIsomorphism: Isomorphism[CursorOp, io.circe.CursorOp] with
    given CanEqual[io.circe.CursorOp, io.circe.CursorOp] = CanEqual.derived
    def to(a: CursorOp): io.circe.CursorOp = a match
      case CursorOp.MoveLeft => io.circe.CursorOp.MoveLeft
      case CursorOp.MoveRight => io.circe.CursorOp.MoveRight
      case CursorOp.MoveUp => io.circe.CursorOp.MoveUp
      case CursorOp.Field(k) => io.circe.CursorOp.Field(k)
      case CursorOp.DownField(k) => io.circe.CursorOp.DownField(k)
      case CursorOp.DownArray => io.circe.CursorOp.DownArray
      case CursorOp.DownN(n) => io.circe.CursorOp.DownN(n)
      case CursorOp.DeleteGoParent => io.circe.CursorOp.DeleteGoParent
    def from(b: io.circe.CursorOp): CursorOp = b match
      case io.circe.CursorOp.MoveLeft => CursorOp.MoveLeft
      case io.circe.CursorOp.MoveRight => CursorOp.MoveRight
      case io.circe.CursorOp.MoveUp => CursorOp.MoveUp
      case io.circe.CursorOp.Field(k) => CursorOp.Field(k)
      case io.circe.CursorOp.DownField(k) => CursorOp.DownField(k)
      case io.circe.CursorOp.DownArray => CursorOp.DownArray
      case io.circe.CursorOp.DownN(n) => CursorOp.DownN(n)
      case io.circe.CursorOp.DeleteGoParent => CursorOp.DeleteGoParent
  end cursorOpIsomorphism

  given successCursorIsomorphism: Isomorphism[SuccessCursor[Json], HCursor] with
    def to(a: SuccessCursor[Json]): HCursor = a match
      case Cursor.TCursor(value, lastCursor, lastOp) =>
        CursorOps.topCursor(value, lastCursor.map(to).orNull, lastOp.map(cursorOpIsomorphism.to).orNull)
      case Cursor.OCursor(obj: JsonObject, keyValue, parent, changed, _, lastCursor, lastOp) =>
        CursorOps.objectCursor(obj, keyValue, to(parent), changed, lastCursor.map(to).orNull,
          lastOp.map(cursorOpIsomorphism.to).orNull)
      case Cursor.ACursor(array, indexValue, parent, changed, _, lastCursor, lastOp) =>
        CursorOps.arrayCursor(array, indexValue, to(parent), changed, lastCursor.map(to).orNull,
          lastOp.map(cursorOpIsomorphism.to).orNull)
    def from(b: HCursor): SuccessCursor[Json] = Cursor.TCursor(b.value, None, None)
  end successCursorIsomorphism
  given failedCursorIsomorphism: Isomorphism[FailedCursor[Json], io.circe.FailedCursor] with
    def to(a: FailedCursor[Json]): io.circe.FailedCursor =
      io.circe.FailedCursor(
        a.lastCursor.map(successCursorIsomorphism.to).orNull,
        a.lastOp.map(cursorOpIsomorphism.to).orNull
      )
    def from(b: io.circe.FailedCursor): FailedCursor[Json] = Cursor.FCursor(None, None)
  end failedCursorIsomorphism
  given cursorIsomorphism: Isomorphism[Cursor[Json], ACursor] with
    def to(a: Cursor[Json]): ACursor = a match
      case cursor: SuccessCursor[Json] => successCursorIsomorphism.to(cursor)
      case cursor: FailedCursor[Json] => failedCursorIsomorphism.to(cursor)
    def from(b: ACursor): Cursor[Json] = b match
      case cursor: HCursor => successCursorIsomorphism.from(cursor)
      case cursor: io.circe.FailedCursor => failedCursorIsomorphism.from(cursor)
  end cursorIsomorphism
  given decodingFailureIsomorphism: Isomorphism[DecodingFailure, io.circe.DecodingFailure] with
    def to(failure: DecodingFailure): io.circe.DecodingFailure = failure match
      case Common(e: WrongType, _, _, Some(value: Json), _, _, Some(history)) =>
        io.circe.DecodingFailure(WrongTypeExpectation(e.expectedType, value), history.map(cursorOpIsomorphism.to))
      case Common(e: Error, _, _, _, _, _, Some(history)) =>
        io.circe.DecodingFailure(e.message, history.map(cursorOpIsomorphism.to))
      case Common(e, _, _, _, _, _, Some(history)) =>
        io.circe.DecodingFailure(Error.pureMessage(e), history.map(cursorOpIsomorphism.to))
      case e: Lift[?] if e.error.isInstanceOf[io.circe.DecodingFailure] => e.error.asInstanceOf[io.circe.DecodingFailure]
      case _ => io.circe.DecodingFailure(failure.message, List.empty[io.circe.CursorOp])
    def from(b: io.circe.DecodingFailure): DecodingFailure = DecodingFailure(b)
  end decodingFailureIsomorphism
  given encoderIsomorphism[A]: Isomorphism[Encoder[Json, A], io.circe.Encoder[A]] with
    def to(a: Encoder[Json, A]): io.circe.Encoder[A] = com.peknight.codec.circe.Encoder(a)
    def from(b: io.circe.Encoder[A]): Encoder[Json, A] = b match
      case e: com.peknight.codec.circe.Encoder[A] => e.encoder
      case e: ConfiguredEncoder[A] if SumOrProductOps.isSum(e) =>
        new SumEncoder[Id, Json, A]:
          def encode(a: A): Json = b(a)
      case e => e(_)
  end encoderIsomorphism

  private[this] def migrateDecoder[T](b: io.circe.Decoder[T]): Decoder[Json, T] = b match
    case d: com.peknight.codec.circe.Decoder[T] => d.decoder
    case d: ConfiguredDecoder[T] if SumOrProductOps.isSum(d) =>
      new SumDecoder[Id, Cursor[Json], DecodingFailure, T]:
        def decoders: Map[String, Decoder[Json, _]] =
          d.constructorNames.zip(d.elemDecoders).map((key, dd) => (key, migrateDecoder(dd))).toMap
        def decode(cursor: Cursor[Json]): Either[DecodingFailure, T] =
          d.tryDecode(cursorIsomorphism.to(cursor)).left.map(decodingFailureIsomorphism.from)
        def decodeAccumulating(cursor: Cursor[Json]): ValidatedNel[DecodingFailure, T] =
          d.tryDecodeAccumulating(cursorIsomorphism.to(cursor)).leftMap(_.map(decodingFailureIsomorphism.from))
    case d =>
      new Decoder[Json, T]:
        def decode(cursor: Cursor[Json]): Either[DecodingFailure, T] =
          d.tryDecode(cursorIsomorphism.to(cursor)).left.map(decodingFailureIsomorphism.from)
        def decodeAccumulating(cursor: Cursor[Json]): ValidatedNel[DecodingFailure, T] =
          d.tryDecodeAccumulating(cursorIsomorphism.to(cursor)).leftMap(_.map(decodingFailureIsomorphism.from))
  end migrateDecoder

  given decoderIsomorphism[A]: Isomorphism[Decoder[Json, A], io.circe.Decoder[A]] with
    def to(a: Decoder[Json, A]): io.circe.Decoder[A] = com.peknight.codec.circe.Decoder(a)
    def from(b: io.circe.Decoder[A]): Decoder[Json, A] = migrateDecoder(b)
  end decoderIsomorphism


  given codecIsomorphism[A]: Isomorphism[Codec[Json, A], io.circe.Codec[A]] with
    def to(a: Codec[Json, A]): io.circe.Codec[A] = com.peknight.codec.circe.Codec(a)
    def from(b: io.circe.Codec[A]): Codec[Json, A] = b match
      case c: com.peknight.codec.circe.Codec[A] => c.codec
      case c: ConfiguredCodec[A] if SumOrProductOps.isSum(c) =>
        new Codec[Json, A] with SumEncoder[Id, Json, A]
          with SumDecoder[Id, Cursor[Json], DecodingFailure, A]:
          def decoders: Map[String, Decoder[Json, _]] =
            c.constructorNames.zip(c.elemDecoders).map((key, dd) => (key, migrateDecoder(dd))).toMap
          def encode(a: A): Json = c(a)
          def decode(cursor: Cursor[Json]): Either[DecodingFailure, A] =
            c.tryDecode(cursorIsomorphism.to(cursor)).left.map(decodingFailureIsomorphism.from)
          def decodeAccumulating(cursor: Cursor[Json]): ValidatedNel[DecodingFailure, A] =
            c.tryDecodeAccumulating(cursorIsomorphism.to(cursor)).leftMap(_.map(decodingFailureIsomorphism.from))
      case c =>
        new Codec[Json, A]:
          def encode(a: A): Json = c.apply(a)
          def decode(cursor: Cursor[Json]): Either[DecodingFailure, A] =
            c.tryDecode(cursorIsomorphism.to(cursor)).left.map(decodingFailureIsomorphism.from)
          def decodeAccumulating(cursor: Cursor[Json]): ValidatedNel[DecodingFailure, A] =
            c.tryDecodeAccumulating(cursorIsomorphism.to(cursor)).leftMap(_.map(decodingFailureIsomorphism.from))
  end codecIsomorphism

  def encoder[A](using encoder: Encoder[Json, A]): io.circe.Encoder[A] = encoderIsomorphism.to(encoder)
  def decoder[A](using decoder: Decoder[Json, A]): io.circe.Decoder[A] = decoderIsomorphism.to(decoder)
  def codec[A](using encoder: Encoder[Json, A], decoder: Decoder[Json, A]): io.circe.Codec[A] =
    codecIsomorphism.to(com.peknight.codec.Codec(encoder, decoder))
end iso
