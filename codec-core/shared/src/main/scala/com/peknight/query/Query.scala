package com.peknight.query

import cats.Id
import com.peknight.codec.Object
import com.peknight.generic.migration.id.Isomorphism

sealed trait Query derives CanEqual:
  def fold[X](queryNull: => X, queryValue: String => X, queryArray: Vector[Query] => X, queryObject: Object[Query] => X)
  : X =
    this match
      case Query.QNull => queryNull
      case Query.QValue(value) => queryValue(value)
      case Query.QArray(value) => queryArray(value)
      case Query.QObject(value) => queryObject(value)
  def isNull: Boolean =
    this match
      case Query.QNull => true
      case _ => false
  def isValue: Boolean =
    this match
      case Query.QValue(_) => true
      case _ => false
  def isArray: Boolean =
    this match
      case Query.QArray(_) => true
      case _ => false
  def isObject: Boolean =
    this match
      case Query.QObject(_) => true
      case _ => false
  def asNull: Option[Unit] =
    this match
      case Query.QNull => Some(())
      case _ => None
  def asValue: Option[String] =
    this match
      case Query.QValue(value) => Some(value)
      case _ => None
  def asArray: Option[Vector[Query]] =
    this match
      case Query.QArray(value) => Some(value)
      case _ => None
  def asObject: Option[Object[Query]] =
    this match
      case Query.QObject(value) => Some(value)
      case _ => None
  def withNull(f: => Query): Query =
    this match
      case Query.QNull => f
      case _ => this
  def withValue(f: String => Query): Query =
    this match
      case Query.QValue(value) => f(value)
      case _ => this
  def withArray(f: Vector[Query] => Query): Query =
    this match
      case Query.QArray(value) => f(value)
      case _ => this
  def withObject(f: Object[Query] => Query): Query =
    this match
      case Query.QObject(value) => f(value)
      case _ => this
  def mapValue(f: String => String): Query =
    this match
      case Query.QValue(value) => Query.QValue(f(value))
      case _ => this
  def mapArray(f: Vector[Query] => Vector[Query]): Query =
    this match
      case Query.QArray(value) => Query.QArray(f(value))
      case _ => this
  def mapObject(f: Object[Query] => Object[Query]): Query =
    this match
      case Query.QObject(value) => Query.QObject(f(value))
      case _ => this
end Query
object Query:
  case object QNull extends Query
  case class QValue(value: String) extends Query
  object QValue:
    given Isomorphism[QValue, String] with
      def to(a: QValue): Id[String] = a.value
      def from(b: String): Id[QValue] = QValue(b)
    end given
  end QValue
  case class QArray(value: Vector[Query]) extends Query
  case class QObject(value: Object[Query]) extends Query
  object QObject:
    given Isomorphism[QObject, Object[Query]] with
      def to(a: QObject): Id[Object[Query]] = a.value
      def from(b: Object[Query]): Id[QObject] = QObject(b)
  end QObject

  val Null: Query = QNull
  def obj(fields: (String, Query)*): Query = fromFields(fields)
  def arr(values: Query*): Query = fromValues(values)
  def fromFields(fields: Iterable[(String, Query)]): Query = QObject(Object.fromIterable(fields))
  def fromValues(values: Iterable[Query]): Query = QArray(values.toVector)
  def fromObject(value: Object[Query]): Query = QObject(value)
  def fromString(value: String): Query = QValue(value)
end Query
