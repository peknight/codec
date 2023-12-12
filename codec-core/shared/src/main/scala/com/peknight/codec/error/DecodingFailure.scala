package com.peknight.codec.error

import cats.Semigroup
import cats.data.NonEmptyList
import cats.syntax.option.*
import com.peknight.codec.cursor.{Cursor, CursorOp, CursorType}
import com.peknight.codec.error.DecodingFailure.DecodingCommonFailure
import com.peknight.error
import com.peknight.error.Error.{Lift, Value}

trait DecodingFailure extends com.peknight.error.codec.DecodingFailure:
  override def label(label: String): DecodingFailure =
    val labelOpt = label.some.filter(_.nonEmpty)
    if labelOpt == labelOption then this
    else
      this match
        case common: DecodingCommonFailure[_] => common.copy(label = labelOpt)
        case e => DecodingCommonFailure(e, labelOpt, None, None, None)
  end label
  override def value[S](value: S): DecodingFailure =
    this match
      case common: DecodingCommonFailure[_] => common.copy(value = value.some)
      case e => DecodingCommonFailure(e, None, Some(value), None, None)
  end value
  def cursor[S](value: Option[S], pathString: String, history: List[CursorOp]): DecodingFailure =
    this match
      case common: DecodingCommonFailure[_] =>
        common.copy(value = value.some, pathString = pathString.some, history = history.some)
      case e => DecodingCommonFailure(e, None, value.some, pathString.some, history.some)
  end cursor
  def cursor[S](t: Cursor[S]): DecodingFailure =
    cursor[S](t.focus, t.pathString, t.history)
  def cursorType[T, S](t: T)(using cursorType: CursorType.Aux[T, S]): DecodingFailure =
    cursor[S](cursorType.focus(t), cursorType.pathString(t), cursorType.history(t))
  end cursorType
end DecodingFailure
object DecodingFailure:
  case class DecodingFailures(errors: NonEmptyList[DecodingFailure]) extends DecodingFailure:
    override protected def lowPriorityMessage: Option[String] = messages.mkString(", ").some
    override def messages: List[String] =errors.toList.flatMap(_.messages)
  end DecodingFailures
  object DecodingFailures:
    def apply(head: DecodingFailure, tail: List[DecodingFailure]): DecodingFailures =
      DecodingFailures(NonEmptyList(head, tail))
    def apply(head: DecodingFailure, tail: DecodingFailure*): DecodingFailures =
      DecodingFailures(NonEmptyList.of(head, tail *))
  end DecodingFailures
  case class DecodingCommonFailure[S](error: DecodingFailure, label: Option[String], value: Option[S],
                                      pathString: Option[String], history: Option[List[CursorOp]])
    extends DecodingFailure with Lift[DecodingFailure] with Value[Option[S]]:
    override protected def labelOption: Option[String] =
      val labelOpt = label.filter(_.nonEmpty)
      val pathOpt = pathString.map(_.replaceFirst("^\\.", "")).filter(_.nonEmpty)
      labelOpt.fold(pathOpt)(lab => pathOpt.fold(lab.some)(path => s"$lab.$path".some))
    override protected def lowPriorityMessage: Option[String] = error.message.some
  end DecodingCommonFailure
  given Semigroup[DecodingFailure] with
    def combine(x: DecodingFailure, y: DecodingFailure): DecodingFailure = (x, y) match
      case (DecodingFailures(xErrors), DecodingFailures(yErrors)) => DecodingFailures(xErrors ++ yErrors.toList)
      case (DecodingFailures(xErrors), yError) => DecodingFailures(xErrors.head, xErrors.tail :+ yError)
      case (xError, DecodingFailures(yErrors)) => DecodingFailures(xError, yErrors.toList)
      case (xError, yError) => DecodingFailures(xError, yError)
  end given
end DecodingFailure
