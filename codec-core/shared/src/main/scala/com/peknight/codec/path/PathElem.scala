package com.peknight.codec.path

sealed trait PathElem
object PathElem:
  case class ObjectKey(keyName: String) extends PathElem
  case class ArrayIndex(index: Long) extends PathElem
  object ArrayIndex:
    def apply(value: Int): ArrayIndex = ArrayIndex(value.toLong)
  end ArrayIndex
end PathElem
