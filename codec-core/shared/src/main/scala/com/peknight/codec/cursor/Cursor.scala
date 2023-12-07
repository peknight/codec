package com.peknight.codec.cursor

import cats.syntax.either.*
import cats.syntax.functor.*
import cats.{Applicative, Eq, Functor}
import com.peknight.codec.error.{CouldNotDecode, DecodingFailure, MissingField}
import com.peknight.codec.path.{PathElem, PathToRoot}
import com.peknight.codec.sum.{ArrayType, ObjectType}

import scala.annotation.tailrec

trait Cursor[S]:
  protected[codec] def lastCursor: Option[SuccessCursor[S]]
  protected[codec] def lastOp: Option[CursorOp]
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
  def values(using ArrayType[S]): Option[Iterable[S]]

  /**
   * If the focus is a value in a S array, return the key.
   */
  def index: Option[Int]

  /**
   * If the focus is a S object, return tis field names in their original order.
   */
  def keys(using ObjectType[S]): Option[Iterable[String]]

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
  def downArray(using ArrayType[S]): Cursor[S]

  /**
   * If the focus is a S array, move to the element at the given index.
   */
  def downN(n: Int)(using ArrayType[S]): Cursor[S]

  /**
   * If the focus is a value in a S object, move to a sibling with the given key.
   */
  def field(k: String): Cursor[S]

  /**
   * If the focus is a S object, move to the value of the given key.
   */
  def downField(k: String)(using ObjectType[S]): Cursor[S]

  private[codec] def pathToRoot: PathToRoot =
    def lastCursorParentOrLastCursor(cursor: Cursor[S]): Option[Cursor[S]] =
      cursor.lastCursor.map {
        case lastCursor: ArrayCursor[S] => lastCursor.parent
        case lastCursor: ObjectCursor[S, _] => lastCursor.parent
        case lastCursor => lastCursor
      }
    @tailrec def go(cursorOption: Option[Cursor[S]], acc: PathToRoot): PathToRoot =
      cursorOption match
        case None => acc
        case Some(cursor) =>
          if cursor.failed then
            /*
             * If the cursor is in a failed state, we lose context on what the
             * attempted last position was. Since we usually want to know this
             * for error reporting, we use the lastOp to attempt to recover that
             * state. We only care about operations which imply a path to the
             * root, such as a field selection.
             */
            cursor.lastOp match
              case Some(CursorOp.Field(field)) =>
                go(lastCursorParentOrLastCursor(cursor), PathElem.ObjectKey(field) +: acc)
              case Some(CursorOp.DownField(field)) =>
                // We tried to move down, and then that failed, so the field was missing.
                go(lastCursorParentOrLastCursor(cursor), PathElem.ObjectKey(field) +: acc)
              case Some(CursorOp.DownArray) =>
                // We tried to move into an array, but it must have been empty.
                go(lastCursorParentOrLastCursor(cursor), PathElem.ArrayIndex(0) +: acc)
              case Some(CursorOp.DownN(n)) =>
                // We tried to move into an array at index N, but there was no element there.
                go(lastCursorParentOrLastCursor(cursor), PathElem.ArrayIndex(n) +: acc)
              case Some(CursorOp.MoveLeft) =>
                // We tried to move to before the start of the array.
                go(lastCursorParentOrLastCursor(cursor), PathElem.ArrayIndex(-1) +: acc)
              case Some(CursorOp.MoveRight) =>
                cursor.lastCursor match
                  case Some(lastCursor: ArrayCursor[S]) =>
                    /*
                     * We tried to move to past the end of the array. Longs are
                     * used here for the very unlikely case that we tried to
                     * move past Int.MaxValue which shouldn't be representable.
                     */
                    go(Some(lastCursor.parent), PathElem.ArrayIndex(lastCursor.indexValue.toLong + 1L) +: acc)
                  // Invalid state
                  case _ => go(cursor.lastCursor, acc)
              case _ =>
                /*
                 * CursorOp.MoveUp or CursorOp.DeleteGoParent, both are move up
                 * events.
                 *
                 * Recalling we are in a failed branch here, this should only
                 * fail if we are already at the top of the tree or if the
                 * cursor state is broken (will be fixed on 0.15.x), in either
                 * case this is the only valid action to take.
                 */
                go(cursor.lastCursor, acc)
          else
            cursor match
              case cursor: ArrayCursor[S] => go(Some(cursor.parent), PathElem.ArrayIndex(cursor.indexValue) +: acc)
              case cursor: ObjectCursor[S, _] => go(Some(cursor.parent), PathElem.ObjectKey(cursor.keyValue) +: acc)
              case cursor: TopCursor[S] => acc
              case _ => go(cursor.lastCursor, acc)
    go(Some(this), PathToRoot.empty)
  end pathToRoot

  /**
   * Creates a JavaScript-style path string, e.g. ".foo.bar[3]".
   */
  def pathString: String = PathToRoot.toPathString(pathToRoot)


  /**
   * Attempt to decode the focus as an A.
   */
  def as[F[_], A](using d: com.peknight.codec.Decoder[F, Cursor[S], DecodingFailure[Cursor[S]], A])
  : F[Either[DecodingFailure[Cursor[S]], A]] =
    d.decode(this)

  /**
   * Attempt to decode the value at the given key in a S object as an A
   */
  def get[F[_], A](k: String)
                  (using com.peknight.codec.Decoder[F, Cursor[S], DecodingFailure[Cursor[S]], A], ObjectType[S])
  : F[Either[DecodingFailure[Cursor[S]], A]] =
    downField(k).as[F, A]

  /**
   * Attempt to decode the value at the given key in a S object as an A. If the field k is missing,
   * then use the fallback instead.
   */
  def getOrElse[F[_]: Functor, A](k: String)(fallback: => A)
                                 (using com.peknight.codec.Decoder[F, Cursor[S], DecodingFailure[Cursor[S]], Option[A]],
                                  ObjectType[S]): F[Either[DecodingFailure[Cursor[S]], A]] =
    get[F, Option[A]](k).map {
      case Right(Some(a)) => a.asRight[DecodingFailure[Cursor[S]]]
      case Right(None) => fallback.asRight[DecodingFailure[Cursor[S]]]
      case left => left.asInstanceOf[Either[DecodingFailure[Cursor[S]], A]]
    }

  /**
   * Replay an operation against this cursor.
   */
  def replayOne(op: CursorOp)(using ObjectType[S], ArrayType[S]): Cursor[S] = op match
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
  def replay(history: List[CursorOp])(using ObjectType[S], ArrayType[S]): Cursor[S] =
    history.foldRight(this)((op, c) => c.replayOne(op))

  def toDecodingFailure: DecodingFailure[Cursor[S]] =
    this match
      case cursor: FailedCursor[S] if cursor.missingField => MissingField(cursor)
      case _ => CouldNotDecode(this)
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
  def from[S](value: S): SuccessCursor[S] = TopCursor(value, None, None)
end Cursor

