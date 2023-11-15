package com.peknight.codec.derivation

import cats.{Eval, Foldable}

trait EncodeObjectOps[S]:
  def empty: S
  def prepended(s: S, field: (String, S)): S
  def contains(s: S, key: String): Boolean
  def singleton(label: String, value: S): S = prepended(empty, (label, value))
  def fromFoldable[F[_] : Foldable](f: F[(String, S)]): S =
    Foldable[F].foldRight(f, Eval.later(empty))((field, e) => e.map(s => prepended(s, field))).value
end EncodeObjectOps

