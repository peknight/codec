package com.peknight.codec.base

import cats.Applicative
import cats.parse.Parser0
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
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

  def codecBaseStringS[F[_]: Applicative, S: StringType]: Codec[F, S, Cursor[S], ByteVector] =
    codecBaseStringSWithAlphabet[F, S](alphabet)

  def baseStringParser: Parser0[String] =
    baseStringParserWithAlphabet(alphabet)

  def baseParser: Parser0[B] = baseParserWithAlphabet(alphabet)

  given stringCodecBase[F[_]: Applicative]: Codec[F, String, String, B] =
    stringCodecBaseWithAlphabet[F](alphabet)

  given codecBaseS[F[_]: Applicative, S: StringType]: Codec[F, S, Cursor[S], B] =
    codecBaseSWithAlphabet[F, S](alphabet)
end BaseAlphabetPlatform
