package com.peknight.codec.http4s.instances

import com.peknight.codec.id.Decoder
import org.http4s.{ParseFailure, QueryParamDecoder, Uri}

import java.time.{Period, ZoneId}

trait QueryParamDecoderInstances extends QueryParamDecoderLowPriorityInstances:
  extension [T] (queryParamDecoder: QueryParamDecoder[T])
    private[this] def to[A](using decoder: Decoder[T, ParseFailure, A]): QueryParamDecoder[A] =
      queryParamDecoder.emapValidatedNel(decoder.decodeAccumulating)
  end extension
  given unitDecoder[A](using Decoder[Unit, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.unitQueryParamDecoder.to[A]
  given booleanDecoder[A](using Decoder[Boolean, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.booleanQueryParamDecoder.to[A]
  given doubleDecoder[A](using Decoder[Double, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.doubleQueryParamDecoder.to[A]
  given floatDecoder[A](using Decoder[Float, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.floatQueryParamDecoder.to[A]
  given shortDecoder[A](using Decoder[Short, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.shortQueryParamDecoder.to[A]
  given intDecoder[A](using Decoder[Int, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.intQueryParamDecoder.to[A]
  given longDecoder[A](using Decoder[Long, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.longQueryParamDecoder.to[A]
  given charDecoder[A](using Decoder[Char, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.charQueryParamDecoder.to[A]
  given stringDecoder[A](using Decoder[String, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.stringQueryParamDecoder.to[A]
  given uriDecoder[A](using Decoder[Uri, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.uriQueryParamDecoder.to[A]
  given zoneIdDecoder[A](using Decoder[ZoneId, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.zoneId.to[A]
  given periodDecoder[A](using Decoder[Period, ParseFailure, A]): QueryParamDecoder[A] =
    QueryParamDecoder.period.to[A]
end QueryParamDecoderInstances
object QueryParamDecoderInstances extends QueryParamDecoderInstances
