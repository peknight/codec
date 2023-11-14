package com.peknight.codec.http4s.instances

import com.peknight.codec.id.Encoder
import com.peknight.codec.syntax.decoder.decodeTo
import org.http4s.{QueryParamDecoder, Uri}

import java.time.{Period, ZoneId}

trait QueryParamDecoderLowPriorityInstances:
  given unitDecoder[A](using Encoder[A, Unit]): QueryParamDecoder[A] =
    QueryParamDecoder.unitQueryParamDecoder.decodeTo[A]
  given booleanDecoder[A](using Encoder[A, Boolean]): QueryParamDecoder[A] =
    QueryParamDecoder.booleanQueryParamDecoder.decodeTo[A]
  given doubleDecoder[A](using Encoder[A, Double]): QueryParamDecoder[A] =
    QueryParamDecoder.doubleQueryParamDecoder.decodeTo[A]
  given floatDecoder[A](using Encoder[A, Float]): QueryParamDecoder[A] =
    QueryParamDecoder.floatQueryParamDecoder.decodeTo[A]
  given shortDecoder[A](using Encoder[A, Short]): QueryParamDecoder[A] =
    QueryParamDecoder.shortQueryParamDecoder.decodeTo[A]
  given intDecoder[A](using Encoder[A, Int]): QueryParamDecoder[A] =
    QueryParamDecoder.intQueryParamDecoder.decodeTo[A]
  given longDecoder[A](using Encoder[A, Long]): QueryParamDecoder[A] =
    QueryParamDecoder.longQueryParamDecoder.decodeTo[A]
  given charDecoder[A](using Encoder[A, Char]): QueryParamDecoder[A] =
    QueryParamDecoder.charQueryParamDecoder.decodeTo[A]
  given stringDecoder[A](using Encoder[A, String]): QueryParamDecoder[A] =
    QueryParamDecoder.stringQueryParamDecoder.decodeTo[A]
  given uriDecoder[A](using Encoder[A, Uri]): QueryParamDecoder[A] =
    QueryParamDecoder.uriQueryParamDecoder.decodeTo[A]
  given zoneIdDecoder[A](using Encoder[A, ZoneId]): QueryParamDecoder[A] =
    QueryParamDecoder.zoneId.decodeTo[A]
  given periodDecoder[A](using Encoder[A, Period]): QueryParamDecoder[A] =
    QueryParamDecoder.period.decodeTo[A]
end QueryParamDecoderLowPriorityInstances
