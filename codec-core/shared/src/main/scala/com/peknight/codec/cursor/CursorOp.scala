package com.peknight.codec.cursor

import cats.instances.list.catsKernelStdEqForList
import cats.{Eq, Show}

sealed trait CursorOp derives CanEqual:
  def requiresObject: Boolean
  def requiresArray: Boolean
end CursorOp
object CursorOp:
  sealed trait ObjectOp extends CursorOp:
    def requiresObject: Boolean = true
    def requiresArray: Boolean = false
  end ObjectOp

  case class DownField(k: String) extends ObjectOp

  sealed trait ArrayOp extends CursorOp:
    def requiresObject: Boolean = false
    def requiresArray: Boolean = true
  end ArrayOp

  case object DownArray extends ArrayOp
  case class DownN(n: Int) extends ArrayOp

  sealed trait UnconstrainedOp extends CursorOp:
    def requiresObject: Boolean = false
    def requiresArray: Boolean = false
  end UnconstrainedOp

  case object MoveLeft extends UnconstrainedOp
  case object MoveRight extends UnconstrainedOp
  case object MoveUp extends UnconstrainedOp
  case class Field(k: String) extends UnconstrainedOp
  case object DeleteGoParent extends UnconstrainedOp

  given Show[CursorOp] with
    def show(t: CursorOp): String = t match
      case MoveLeft => "<-"
      case MoveRight => "->"
      case MoveUp => "_/"
      case Field(f) => "--(" + f + ")"
      case DownField(f) => "--\\(" + f + ")"
      case DownArray => "\\\\"
      case DownN(n) => "=\\(" + n + ")"
      case DeleteGoParent => "!_/"
  end given
  given Eq[CursorOp] = Eq.fromUniversalEquals

  val eqCursorOpList: Eq[List[CursorOp]] = catsKernelStdEqForList[CursorOp]

  private[this] sealed trait Selection
  private[this] case class SelectField(field: String) extends Selection
  private[this] case class SelectIndex(index: Int) extends Selection
  private[this] case class Op(op: CursorOp) extends Selection
  def opsToPath(history: List[CursorOp]): String =
    val selections = history.foldRight(List.empty[Selection]) {
      case (DownField(k), acc) => SelectField(k) :: acc
      case (DownArray, acc) => SelectIndex(0) :: acc
      case (MoveUp, _ :: tail) => tail
      case (MoveRight, SelectIndex(i) :: tail) => SelectIndex(i + 1) :: tail
      case (MoveLeft, SelectIndex(i) :: tail) => SelectIndex(i - 1) :: tail
      case (op, acc) => Op(op) :: acc
    }
    selections.foldLeft("") {
      case (str, SelectField(f)) => s".$f$str"
      case (str, SelectIndex(i)) => s"[$i]$str"
      case (str, Op(op)) => s"{${Show[CursorOp].show(op)}$str"
    }
  end opsToPath
end CursorOp
