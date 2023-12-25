package com.peknight.codec.path

import com.peknight.codec.cursor.CursorOp
import com.peknight.codec.path.PathElem.{ArrayIndex, ObjectKey}
import com.peknight.error.Error
import cats.syntax.eq.*
import cats.syntax.foldable.*

case class PathToRoot private(value: Vector[PathElem]) extends AnyVal:
  def asPathString: String = PathToRoot.toPathString(this)
  def prependElem(elem: PathElem): PathToRoot = PathToRoot(elem +: value)
  def appendElem(elem: PathElem): PathToRoot = PathToRoot(value :+ elem)
  def +:(elem: PathElem): PathToRoot = prependElem(elem)
  def :+(elem: PathElem): PathToRoot = appendElem(elem)
end PathToRoot
object PathToRoot:
  val empty: PathToRoot = PathToRoot(Vector.empty)
  def toPathString(path: PathToRoot): String =
    if path.value.isEmpty then ""
    else
      path.value
        .foldLeft(new StringBuilder(path.value.size * 5)) {
          case (sb, ObjectKey(keyName)) => sb.append(".").append(keyName)
          case (sb, ArrayIndex(index)) => sb.append("[").append(index.toString).append("]")
        }
        .toString
  end toPathString

  def fromHistory(ops: List[CursorOp]): Either[PathToRootError, PathToRoot] =
    ops.reverse
      .foldM[[A] =>> Either[PathToRootError, A], Vector[PathElem]](Vector.empty[PathElem]) {
        // MoveLeft
        case (acc :+ ArrayIndex(n), CursorOp.MoveLeft) if n <= 0 => Left(MoveBeyondBeginning)
        case (acc :+ ArrayIndex(n), CursorOp.MoveLeft) => Right(acc :+ ArrayIndex(n - 1))
        // MoveRight
        case (acc :+ ArrayIndex(n), CursorOp.MoveRight) if n === Int.MaxValue => Left(MoveOutIntMaxValue)
        case (acc :+ ArrayIndex(n), CursorOp.MoveRight) => Right(acc :+ ArrayIndex(n + 1))
        // MoveUp
        case (acc :+ _, CursorOp.MoveUp) => Right(acc)
        case (acc, CursorOp.MoveUp) => Left(MoveUpAboveRoot)
        // Field
        case (acc :+ ObjectKey(_), CursorOp.Field(name)) => Right(acc :+ ObjectKey(name))
        case (_, CursorOp.Field(name)) => Left(MoveSiblingNonObject(name))
        // DownField
        case (acc, CursorOp.DownField(name)) => Right(acc :+ ObjectKey(name))
        // DownArray
        case (acc, CursorOp.DownArray) => Right(acc :+ ArrayIndex(0))
        // DownN
        case (acc, CursorOp.DownN(n)) => Right(acc :+ ArrayIndex(n))
        // DeleteGoParent
        case (acc :+ _, CursorOp.DeleteGoParent) => Right(acc)
        case (acc, CursorOp.DeleteGoParent) => Left(MoveUpAboveRoot)
        // Otherwise
        case (acc, invalid) => Left(InvalidHistoryState(invalid))
      }
      .map(PathToRoot.apply)

  sealed trait PathToRootError(msg: String) extends Error:
    override def lowPriorityMessage: Option[String] = Some(msg)
  end PathToRootError
  private[this] object MoveUpAboveRoot extends PathToRootError("Attempt to move up above the root of the document.")
  private[this] object MoveBeyondBeginning extends PathToRootError(
    "Attempt to move beyond beginning of array in cursor history."
  )
  private[this] object MoveOutIntMaxValue extends PathToRootError(
    "Attempt to move to index > Int.MaxValue in array in cursor history."
  )
  private[this] case class MoveSiblingNonObject(name: String) extends PathToRootError(
    s"Attempt to move to sibling field($name), but cursor history didn't indicate we were in an object."
  )
  private[this] case class InvalidHistoryState(invalid: CursorOp) extends PathToRootError(
    s"Invalid cursor history state: $invalid"
  )
end PathToRoot
