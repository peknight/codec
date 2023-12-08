package com.peknight.codec.error

import scala.reflect.ClassTag

trait WrongClassTag[A, C] extends DecodingFailure[A]
  with com.peknight.error.std.WrongClassTag[C]:
end WrongClassTag
object WrongClassTag:
  private[this] case class WrongClassTag[A, C](value: A, expectedClassTag: ClassTag[C])
    extends com.peknight.codec.error.WrongClassTag[A, C]:
    def map[B](f: A => B): DecodingFailure[B] = WrongClassTag(f(value), expectedClassTag)
  end WrongClassTag

  def apply[A, C](value: A)(using classTag: ClassTag[C]): com.peknight.codec.error.WrongClassTag[A, C] =
    WrongClassTag(value, classTag)
end WrongClassTag
