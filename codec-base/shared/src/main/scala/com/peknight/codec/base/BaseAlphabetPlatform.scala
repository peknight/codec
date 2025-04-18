package com.peknight.codec.base

import cats.parse.Parser0
import cats.{Applicative, Eq, Show}
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
import com.peknight.error.parse.ParsingFailure
import scodec.bits.Bases.Alphabet
import scodec.bits.ByteVector

import scala.reflect.ClassTag

trait BaseAlphabetPlatform[A <: Alphabet, B <: Base : ClassTag] extends BasePlatform[A, B]:
  def alphabet: A
  protected def fromBaseDescriptive: String => Either[String, ByteVector] =
    s => fromBaseDescriptiveWithAlphabet(s, alphabet)
  protected def toBaseString: ByteVector => String = b => toBaseStringWithAlphabet(b, alphabet)
  protected def toBase: String => B
  override protected def toBaseWithAlphabet: (String, A) => B = (s, _) => toBase(s)

  def stringCodecBaseString[F[_]: Applicative]: Codec[F, String, String, ByteVector] =
    stringCodecBaseStringWithAlphabet[F](alphabet)

  def codecBaseStringS[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], ByteVector] =
    codecBaseStringSWithAlphabet[F, S](alphabet)

  def baseStringParser: Parser0[String] =
    baseStringParserWithAlphabet(alphabet)

  def baseParser: Parser0[B] = baseParserWithAlphabet(alphabet)

  given stringCodecBase[F[_]: Applicative]: Codec[F, String, String, B] =
    stringCodecBaseWithAlphabet[F](alphabet)

  given codecBaseS[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], B] =
    codecBaseSWithAlphabet[F, S](alphabet)

  given eqBase: Eq[B] with
    def eqv(x: B, y: B): Boolean = x.value.equals(y.value)
  end eqBase

  def fromString(value: String): Either[ParsingFailure, B] = fromString(value, alphabet)
  def unsafeFromString(value: String): B = unsafeFromString(value, alphabet)
  def fromByteVector(bytes: ByteVector): B = fromByteVector(bytes, alphabet)
  def fromBigInt(value: BigInt): B = fromBigInt(value, alphabet)
  def fromUnsignedBigInt(value: BigInt): B = fromUnsignedBigInt(value, alphabet)
  def fromUnsignedBigInt(value: BigInt, minLength: Int): B = fromUnsignedBigInt(value, minLength, alphabet)
end BaseAlphabetPlatform
