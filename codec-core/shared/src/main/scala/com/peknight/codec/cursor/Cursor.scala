package com.peknight.codec.cursor

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.{Applicative, Eq, Functor}
import com.peknight.codec.cursor.Cursor.{ArrayCursor, FailedCursor, ObjectCursor, SuccessCursor, TopCursor}
import com.peknight.codec.error.{CursorFailure, DecodingFailure, MissingField}
import com.peknight.codec.path.{PathElem, PathToRoot}
import com.peknight.codec.sum.{ArrayType, ObjectType}

import scala.annotation.tailrec

sealed trait Cursor[S]:
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
        case Some(cursor: FailedCursor[S]) =>
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
        case Some(cursor: ArrayCursor[S]) => go(Some(cursor.parent), PathElem.ArrayIndex(cursor.indexValue) +: acc)
        case Some(cursor: ObjectCursor[S, _]) => go(Some(cursor.parent), PathElem.ObjectKey(cursor.keyValue) +: acc)
        case Some(cursor: TopCursor[S]) => acc
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
      case _ => CursorFailure(this)
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

  given [S](using ObjectType[S]): CursorType[Cursor[S]] with
    type Sum = S
    def focus(t: Cursor[S]): Option[S] = t.focus
    def downField(t: Cursor[S], k: String): Cursor[S] = t.downField(k)
  end given

  def from[S](value: S): SuccessCursor[S] = TCursor(value, None, None)

  sealed trait SuccessCursor[S] extends Cursor[S]:
    def value: S
    def replace(newValue: S, cursor: SuccessCursor[S], op: Option[CursorOp]): SuccessCursor[S]
    def addOp(cursor: SuccessCursor[S], op: CursorOp): SuccessCursor[S]
    def withFocus(f: S => S): Cursor[S] = replace(f(value), this, None)
    def withFocusM[F[_] : Applicative](f: S => F[S]): F[Cursor[S]] = f(value).map(replace(_, this, None))
    def succeeded: Boolean = true
    def success: Option[SuccessCursor[S]] = Some(this)
    def focus: Option[S] = Some(value)
    def values(using ArrayType[S]): Option[Iterable[S]] = ArrayType[S].asArray(value)
    def keys(using objectType: ObjectType[S]): Option[Iterable[String]] = objectType.asObject(value).map(objectType.keys)
    protected[this] def up[A](changed: Boolean, parent: SuccessCursor[S], s: S): Cursor[S] =
      if !changed then parent.addOp(this, CursorOp.MoveUp)
      else parent.replace(s, this, Some(CursorOp.MoveUp))

    def top: Option[S] =
      @tailrec def go(cursor: SuccessCursor[S]): Option[S] =
        cursor match
          case topCursor: TopCursor[S] => Some(topCursor.value)
          case _ => go(cursor.up.asInstanceOf[SuccessCursor[S]])
      go(this)

    def root: Option[SuccessCursor[S]] =
      @tailrec def go(cursor: SuccessCursor[S]): Option[SuccessCursor[S]] =
        cursor match
          case topCursor: TopCursor[S] => Some(topCursor)
          case _ => go(cursor.up.asInstanceOf[SuccessCursor[S]])
      go(this)

    def downArray(using ArrayType[S]): Cursor[S] =
      ArrayType[S].asArray(value).filter(_.nonEmpty) match
        case Some(values) =>
          ACursor(values, 0, this, false, ArrayType[S], this, Some(CursorOp.DownArray))
        case _ => fail(CursorOp.DownArray)

    def find(p: S => Boolean): Cursor[S] =
      @tailrec def go(cursor: Cursor[S]): Cursor[S] =
        cursor match
          case success: SuccessCursor[S] => if p(success.value) then success else go(success.right)
          case other => other
      go(this)

    def downField(k: String)(using objectType: ObjectType[S]): Cursor[S] =
      objectType.asObject(value) match
        case Some(o) if objectType.contains(o, k) =>
          OCursor[S, objectType.Obj](o, k, this, false, objectType, this,
            Some(CursorOp.DownField(k)))
        case _ => fail(CursorOp.DownField(k))

    def downN(n: Int)(using ArrayType[S]): Cursor[S] =
      ArrayType[S].asArray(value) match
        case Some(values) if n >= 0 && values.lengthCompare(n) > 0 =>
          ACursor(values, n, this, false, ArrayType[S], this, Some(CursorOp.DownN(n)))
        case _ => fail(CursorOp.DownN(n))

    protected[this] def fail(op: CursorOp): Cursor[S] = FCursor(this, op)
  end SuccessCursor

  type SCursor[S] = SuccessCursor[S]

  private[codec] sealed trait TopCursor[S] extends SuccessCursor[S]:
    def index: Option[Int] = None
    def key: Option[String] = None

    def replace(newValue: S, cursor: SuccessCursor[S], op: Option[CursorOp]): SuccessCursor[S] =
      TCursor(newValue, Some(cursor), op)

    def addOp(cursor: SuccessCursor[S], op: CursorOp): SuccessCursor[S] = TCursor(value, Some(cursor), Some(op))

    def up: Cursor[S] = fail(CursorOp.MoveUp)
    def delete: Cursor[S] = fail(CursorOp.DeleteGoParent)
    def left: Cursor[S] = fail(CursorOp.MoveLeft)
    def right: Cursor[S] = fail(CursorOp.MoveRight)
    def field(k: String): Cursor[S] = fail(CursorOp.Field(k))
  end TopCursor

  private[codec] case class TCursor[S](value: S, lastCursor: Option[SuccessCursor[S]], lastOp: Option[CursorOp])
    extends TopCursor[S]

  private[codec] sealed trait ObjectCursor[S, O] extends SuccessCursor[S]:
    protected[this] def obj: O
    def keyValue: String
    def parent: SuccessCursor[S]
    protected[this] def changed: Boolean
    protected[this] def objectType: ObjectType.Aux[S, O]
    protected[this] def lastCursorValue: SuccessCursor[S]
    def lastCursor: Option[SuccessCursor[S]] = Some(lastCursorValue)
    def value: S = objectType.applyUnsafe(obj, keyValue)
    def index: Option[Int] = None
    def key: Option[String] = Some(keyValue)

    def replace(newValue: S, cursor: SuccessCursor[S], op: Option[CursorOp]): SuccessCursor[S] =
      OCursor(objectType.add(obj, keyValue, newValue), keyValue, parent, true, objectType, cursor, op)

    def addOp(cursor: SuccessCursor[S], op: CursorOp): SuccessCursor[S] =
      OCursor(obj, keyValue, parent, changed, objectType, cursor, Some(op))

    def up: Cursor[S] = up(changed, parent, objectType.to(obj))

    def delete: Cursor[S] =
      parent.replace(objectType.to(objectType.remove(obj, keyValue)), this, Some(CursorOp.DeleteGoParent))

    def field(k: String): Cursor[S] =
      if !objectType.contains(obj, k) then fail(CursorOp.Field(k))
      else OCursor(obj, k, parent, changed, objectType, this, Some(CursorOp.Field(k)))

    def left: Cursor[S] = fail(CursorOp.MoveLeft)
    def right: Cursor[S] = fail(CursorOp.MoveRight)
  end ObjectCursor

  private[codec] case class OCursor[S, O](obj: O, keyValue: String, parent: SuccessCursor[S], changed: Boolean,
                                          objectType: ObjectType.Aux[S, O], lastCursorValue: SuccessCursor[S],
                                          lastOp: Option[CursorOp]) extends ObjectCursor[S, O]

  private[codec] sealed trait ArrayCursor[S] extends SuccessCursor[S]:
    protected[this] def array: Vector[S]
    def indexValue: Int
    def parent: SuccessCursor[S]
    protected[this] def changed: Boolean
    protected[this] def arrayType: ArrayType[S]
    protected[this] def lastCursorValue: SuccessCursor[S]
    def lastCursor: Option[SuccessCursor[S]] = Some(lastCursorValue)
    def value: S = array(indexValue)
    def index: Option[Int] = Some(indexValue)
    def key: Option[String] = None
    private[this] def valuesExcept: Vector[S] = array.take(indexValue) ++ array.drop(indexValue + 1)

    def replace(newValue: S, cursor: SuccessCursor[S], op: Option[CursorOp]): SuccessCursor[S] =
      ACursor(array.updated(indexValue, newValue), indexValue, parent, true, arrayType, cursor, op)

    def addOp(cursor: SuccessCursor[S], op: CursorOp): SuccessCursor[S] =
      ACursor(array, indexValue, parent, changed, arrayType, cursor, Some(op))

    def up: Cursor[S] = up(changed, parent, arrayType.to(array))

    def delete: Cursor[S] =
      parent.replace(arrayType.to(array), this, Some(CursorOp.DeleteGoParent))

    def left: Cursor[S] =
      if indexValue == 0 then fail(CursorOp.MoveLeft)
      else ACursor(array, indexValue - 1, parent, changed, arrayType, this, Some(CursorOp.MoveLeft))

    def right: Cursor[S] =
      if indexValue == array.size - 1 then fail(CursorOp.MoveRight)
      else ACursor(array, indexValue + 1, parent, changed, arrayType, this, Some(CursorOp.MoveRight))

    def field(k: String): Cursor[S] = fail(CursorOp.Field(k))
  end ArrayCursor
  private[codec] case class ACursor[S](array: Vector[S], indexValue: Int, parent: SuccessCursor[S], changed: Boolean,
                                       arrayType: ArrayType[S], lastCursorValue: SuccessCursor[S],
                                       lastOp: Option[CursorOp]) extends ArrayCursor[S]

  sealed trait FailedCursor[S] extends Cursor[S]:
    protected[this] def lastCursorValue: SuccessCursor[S]
    protected[this] def lastOpValue: CursorOp
    protected[codec] def lastCursor: Option[SuccessCursor[S]] = Some(lastCursorValue)
    protected[codec] def lastOp: Option[CursorOp] = Some(lastOpValue)
    def incorrectFocusO(using ObjectType[S]): Boolean =
      lastOpValue.requiresObject && !ObjectType[S].isObject(lastCursorValue.value)
    def incorrectFocusA(using ArrayType[S]): Boolean =
      lastOpValue.requiresArray && !ArrayType[S].isArray(lastCursorValue.value)
    def incorrectFocus(using ObjectType[S], ArrayType[S]): Boolean = incorrectFocusO || incorrectFocusA
    def missingField: Boolean =
      lastOpValue match
        case _: CursorOp.Field | _: CursorOp.DownField => true
        case _ => false
    def succeeded: Boolean = false
    def success: Option[SuccessCursor[S]] = None
    def focus: Option[S] = None
    def top: Option[S] = None
    def root: Option[SuccessCursor[S]] = lastCursorValue.root
    def withFocus(f: S => S): Cursor[S] = this
    def withFocusM[F[_] : Applicative](f: S => F[S]): F[Cursor[S]] = this.pure[F]

    def values(using ArrayType[S]): Option[Iterable[S]] = None

    def index: Option[Int] = None
    def keys(using ObjectType[S]): Option[Iterable[String]] = None
    def key: Option[String] = None
    def downArray(using ArrayType[S]): Cursor[S] = this
    def downField(k: String)(using ObjectType[S]): Cursor[S] = this
    def downN(n: Int)(using ArrayType[S]): Cursor[S] = this
    def up: Cursor[S] = this
    def left: Cursor[S] = this
    def right: Cursor[S] = this
    def last: Cursor[S] = this
    def delete: Cursor[S] = this
    def field(k: String): Cursor[S] = this
  end FailedCursor

  case class FCursor[S](lastCursorValue: SuccessCursor[S], lastOpValue: CursorOp) extends FailedCursor[S]
end Cursor

