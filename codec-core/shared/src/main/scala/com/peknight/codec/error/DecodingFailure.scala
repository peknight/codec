package com.peknight.codec.error

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.{Eval, Monoid, Show}
import com.peknight.codec.cursor.{Cursor, CursorOp}
import com.peknight.codec.error.DecodingFailure.{Common, Pure}
import com.peknight.error.Error
import com.peknight.error.option.OptionEmpty

import scala.annotation.tailrec

trait DecodingFailure extends com.peknight.error.codec.DecodingFailure:
  override protected def pure: DecodingFailure = DecodingFailure.pure(this)

  override protected def lowPriorityLabelMessage(label: String): Option[String] = Some(s"decode $label failed")

  override def label(label: String): DecodingFailure =
    val labelOpt = label.some.filter(_.nonEmpty)
    if labelOpt == labelOption then this
    else
      pure match
        case Pure(e) => Common(e, labelOpt, cause = cause)
        case c @ Common(_, _, _, _, _, _, _) => c.copy(label = labelOpt)
        case _ => Common(this, labelOpt, cause = cause)
  end label

  override def prependLabel(prependLabel: String): DecodingFailure =
    prependLabel.some.filter(_.nonEmpty) match
      case None => this
      case Some(prepend) => label(labelOption.fold(prepend)(lab => s"$prepend.$lab"))
  end prependLabel

  override def message(message: String): DecodingFailure =
    val messageOpt = message.some.filter(_.nonEmpty)
    if messageOpt == messageOption then this
    else
      pure match
        case Pure(e) => Common(e, messageOption = messageOpt, cause = cause)
        case c @ Common(_, _, _, _, _, _, _) => c.copy(messageOption = messageOpt)
        case _ => Common(this, messageOption = messageOpt, cause = cause)
  end message

  override def value[A](value: A)(using show: Show[A]): DecodingFailure = pure match
    case Pure(e) => Common(e, valueShow = (value, Eval.later(show.show(value))).some, cause = cause)
    case c @ Common(_, _, _, _, _, _, _) => c.copy(valueShow = (value, Eval.later(show.show(value))).some)
    case _ => Common(this, valueShow = (value, Eval.later(show.show(value))).some, cause = cause)

  override def prepended[A](value: A)(using show: Show[A]): DecodingFailure = pure match
    case Pure(e) => Common(e, valueShow = (value, Eval.later(show.show(value))).some, cause = cause)
    case c @ Common(_, _, _, None, _, _, _) => c.copy(valueShow = (value, Eval.later(show.show(value))).some)
    case c @ Common(_, _, _, Some((t: Tuple, eval)), _, _, _) =>
      c.copy(valueShow = (value *: t, eval.flatMap(s => Eval.later(s"${show.show(value)},$s"))).some)
    case c @ Common(_, _, _, Some((v, eval)), _, _, _) =>
      c.copy(valueShow = (value *: v *: EmptyTuple, eval.flatMap(s => Eval.later(s"${show.show(value)},$s"))).some)
    case _ => Common(this, valueShow = (value, Eval.later(show.show(value))).some, cause = cause)

  override def *:[A](value: A)(using show: Show[A]): DecodingFailure = prepended(value)

  def cursor[S](value: Option[S], pathString: String, history: List[CursorOp])(using show: Show[S]): DecodingFailure =
    this match
      case Pure(e) =>
        Common(e, valueShow = value.map(v => (v, Eval.later(show.show(v)))), cause = cause,
          pathString = pathString.some, history = history.some)
      case c @ Common(_, _, _, _, _, _, _) =>
        c.copy(valueShow = value.map(v => (v, Eval.later(show.show(v)))), cause = cause, pathString = pathString.some,
          history = history.some)
      case e => Common(e, valueShow = value.map(v => (v, Eval.later(show.show(v)))), cause = cause,
        pathString = pathString.some, history = history.some)
  end cursor
  def cursor[S: Show](t: Cursor[S]): DecodingFailure =
    cursor[S](t.focus, t.pathString, t.history)
end DecodingFailure
object DecodingFailure extends DecodingFailure:
  private[codec] object Success extends DecodingFailure with com.peknight.error.Success
  private[codec] case class Pure[+E](error: E) extends DecodingFailure with com.peknight.error.Pure[E]

  private[codec] case class Errors(errors: NonEmptyList[DecodingFailure]) extends DecodingFailure
    with com.peknight.error.Errors[DecodingFailure]

  private[codec] case class Common[+E, A](
                                           error: E,
                                           label: Option[String] = None,
                                           override val messageOption: Option[String] = None,
                                           valueShow: Option[(A, Eval[String])] = None,
                                           override val cause: Option[Error] = None,
                                           pathString: Option[String] = None,
                                           history: Option[List[CursorOp]] = None
  ) extends DecodingFailure with com.peknight.error.Common[E, A]:
    override protected def labelOption: Option[String] =
      val labelOpt = label.filter(_.nonEmpty)
      val pathOpt = pathString.map(_.replaceFirst("^\\.", "")).filter(_.nonEmpty)
      labelOpt.fold(pathOpt)(lab => pathOpt.fold(lab.some)(path => s"$lab.$path".some))
  end Common

  @tailrec def pure[E](error: E): DecodingFailure =
    error match
      case e: (com.peknight.error.Pure[?] & DecodingFailure) => pure(e.error)
      case e: (com.peknight.error.Errors[?] & DecodingFailure) if e.errors.tail.isEmpty => pure(e.errors.head)
      case e: DecodingFailure => e
      case _ => Pure(error)

  def apply: DecodingFailure = Success

  def apply[E](error: E): DecodingFailure =
    given [A]: CanEqual[List[A], E] = CanEqual.derived
    error match
      case NonEmptyList(head, tail) => apply(head, tail)
      case Nil => Success
      case head :: Nil => pure(head)
      case head :: tail => apply(head, tail)
      case _ => pure(error)

  def apply[E](head: E, tail: E*): DecodingFailure = apply(head, tail.toList)

  def apply[E](head: E, tail: List[E]): DecodingFailure =
    if tail.isEmpty then pure(head)
    else Errors(NonEmptyList(pure(head), tail.map(pure)))

  def isNullError[E](error: E): Boolean =
    Error.base(error) match
      case MissingField => true
      case OptionEmpty => true
      case ReadNone => true
      case _ => false

  given Monoid[DecodingFailure] with
    def empty: DecodingFailure = Success

    def combine(x: DecodingFailure, y: DecodingFailure): DecodingFailure = (pure(x), pure(y)) match
      case (Success, yError) => yError
      case (xError, Success) => xError
      case (Errors(xErrors), Errors(yErrors)) => Errors(xErrors ++ yErrors.toList)
      case (Errors(xErrors), yError) => Errors(NonEmptyList(xErrors.head, xErrors.tail :+ yError))
      case (xError, Errors(yErrors)) => Errors(NonEmptyList(xError, yErrors.toList))
      case (xError, yError) => Errors(NonEmptyList(xError, List(yError)))
  end given
end DecodingFailure
