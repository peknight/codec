package com.peknight.codec.http4s.instances

import com.peknight.codec.id.Encoder
import com.peknight.codec.syntax.encoder.encodeTo
import org.http4s.{QueryParamEncoder, Uri}

import java.time.{Period, ZoneId}

trait QueryParamEncoderInstances:
  given booleanEncoder[A](using Encoder[Boolean, A]): QueryParamEncoder[A] =
    QueryParamEncoder.booleanQueryParamEncoder.encodeTo[A]
  given doubleEncoder[A](using Encoder[Double, A]): QueryParamEncoder[A] =
    QueryParamEncoder.doubleQueryParamEncoder.encodeTo[A]
  given floatEncoder[A](using Encoder[Float, A]): QueryParamEncoder[A] =
    QueryParamEncoder.floatQueryParamEncoder.encodeTo[A]
  given shortEncoder[A](using Encoder[Short, A]): QueryParamEncoder[A] =
    QueryParamEncoder.shortQueryParamEncoder.encodeTo[A]
  given intEncoder[A](using Encoder[Int, A]): QueryParamEncoder[A] =
    QueryParamEncoder.intQueryParamEncoder.encodeTo[A]
  given longEncoder[A](using Encoder[Long, A]): QueryParamEncoder[A] =
    QueryParamEncoder.longQueryParamEncoder.encodeTo[A]
  given stringEncoder[A](using Encoder[String, A]): QueryParamEncoder[A] =
    QueryParamEncoder.stringQueryParamEncoder.encodeTo[A]
  given uriEncoder[A](using Encoder[Uri, A]): QueryParamEncoder[A] =
    QueryParamEncoder.uriQueryParamEncoder.encodeTo[A]
  given zoneIdEncoder[A](using Encoder[ZoneId, A]): QueryParamEncoder[A] =
    QueryParamEncoder.zoneId.encodeTo[A]
  given periodEncoder[A](using Encoder[Period, A]): QueryParamEncoder[A] =
    QueryParamEncoder.period.encodeTo[A]
end QueryParamEncoderInstances
object QueryParamEncoderInstances extends QueryParamEncoderInstances