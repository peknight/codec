package com.peknight.codec.derivation

import cats.{Eval, Foldable}

trait EncodeObjectOps[S]:
  def encodeContains(s: S, key: String): Boolean
  def empty: S
  def singleton(label: String, value: S): S = prepend(empty, (label, value))
  def fromFoldable[F[_] : Foldable](f: F[(String, S)]): S =
    Foldable[F].foldRight(f, Eval.later(empty))((field, e) => e.map(s => prepend(s, field))).value
  def prepend(s: S, field: (String, S)): S
end EncodeObjectOps

