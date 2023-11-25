package io.circe.derivation

object SumOrProductOps:
  def isSum[A](a: A): Boolean = a match
    case sumOrProduct: SumOrProduct => sumOrProduct.isSum
    case _ => false
end SumOrProductOps
