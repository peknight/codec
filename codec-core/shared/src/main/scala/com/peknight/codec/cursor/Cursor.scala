package com.peknight.codec.cursor

import cats.syntax.either.*
import cats.syntax.functor.*
import cats.{Applicative, Eq, Functor}
import com.peknight.codec.Object

import scala.annotation.tailrec

trait Cursor[S]:
  def lastCursor: Option[SuccessCursor[S]]
  def lastOp: Option[CursorOp]
  def focus: Option[S]
  final def history: List[CursorOp] =
    @tailrec def go(cursor: Cursor[S], acc: List[CursorOp]): List[CursorOp] =
      (cursor.lastCursor, cursor.lastOp) match
        case (Some(c), Some(op)) => go(c, op :: acc)
        case (Some(c), None) => go(c, acc)
        case (None, Some(op)) => op :: acc
        case _ => acc
    go(this, List.empty[CursorOp]).reverse

  def succeeded: Boolean
  final def failed: Boolean = !succeeded

  /**
   * Return the cursor as an SuccessCursor if it was successful
   */
  def success: Option[SuccessCursor[S]]

  /**
   * Return to the root of the document
   */
  def top: Option[S]

  /**
   * Return the cursor th the root of the document
   */
  def root: Option[SuccessCursor[S]]

  /**
   * Modify the focus using the given function
   */
  def withFocus(f: S => S): Cursor[S]

  /**
   * Modify the focus in a context using the given function.
   */
  def withFocusM[F[_]: Applicative](f: S => F[S]): F[Cursor[S]]

  /**
   * Replace the focus
   */
  def set(s: S): Cursor[S] = withFocus(_ => s)

  /**
   * If the focus is a S Array, return its elements
   */
  def values: Option[Iterable[S]]

  /**
   * If the focus is a value in a S array, return the key.
   */
  def index: Option[Int]

  /**
   * If the focus is a S object, return tis field names in their original order.
   */
  def keys: Option[Iterable[String]]

  /**
   * If the focus is a value in a S object, return the key.
   */
  def key: Option[String]

  /**
   * Delete the focus and move to its parent
   */
  def delete: Cursor[S]

  /**
   * Move the focus to the parent.
   */
  def up: Cursor[S]

  /**
   * If the focus is an element in a S array, move to the left.
   */
  def left: Cursor[S]

  /**
   * If the focus is an element in a S array, move to the right.
   */
  def right: Cursor[S]

  /**
   * If the focus is a S array, move to its first element.
   */
  def downArray: Cursor[S]

  /**
   * If the focus is a S array, move to the element at the given index.
   */
  def downN(n: Int): Cursor[S]

  /**
   * If the focus is a value in a S object, move to a sibling with the given key.
   */
  def field(k: String): Cursor[S]

  /**
   * If the focus is a S object, move to the value of the given key.
   */
  def downField(k: String): Cursor[S]

  // TODO pathToRoot pathString

  /**
   * Attempt to decode the focus as an A.
   */
  def as[F[_], A](using d: Decoder[F, S, A]): Result[F, S, A] = d.decode(this)

  /**
   * Attempt to decode the value at the given key in a S object as an A
   */
  def get[F[_], A](k: String)(using d: Decoder[F, S, A]): F[Either[DecodingFailure[S], A]] = downField(k).as[F, A]

  /**
   * Attempt to decode the value at the given key in a S object as an A. If the field k is missing,
   * then use the fallback instead.
   */
  def getOrElse[F[_]: Functor, A](k: String)(fallback: => A)(using d: Decoder[F, S, Option[A]]): Result[F, S, A] =
    get[F, Option[A]](k).map {
      case Right(Some(a)) => a.asRight[DecodingFailure[S]]
      case Right(None) => fallback.asRight[DecodingFailure[S]]
      case left => left.asInstanceOf[Either[DecodingFailure[S], A]]
    }

  /**
   * Replay an operation against this cursor.
   */
  def replayOne(op: CursorOp): Cursor[S] = op match
    case CursorOp.MoveLeft => left
    case CursorOp.MoveRight => right
    case CursorOp.MoveUp => up
    case CursorOp.Field(k) => field(k)
    case CursorOp.DownField(k) => downField(k)
    case CursorOp.DownArray => downArray
    case CursorOp.DownN(n) => downN(n)
    case CursorOp.DeleteGoParent => delete

  /**
   * Replay history (a list of operations in reverse "chronological" order) against this cursor.
   */
  def replay(history: List[CursorOp]): Cursor[S] = history.foldRight(this)((op, c) => c.replayOne(op))
end Cursor
object Cursor:
  given [S](using eq: Eq[S]): Eq[Cursor[S]] with
    def eqv(x: Cursor[S], y: Cursor[S]): Boolean =
      val focusEq = (x.focus, y.focus) match
        case (Some(a), Some(b)) => eq.eqv(a, b)
        case (None, None) => true
        case _ => false
      val historyEq = CursorOp.eqCursorOpList.eqv(x.history, y.history)
      focusEq && historyEq
  end given
end Cursor

