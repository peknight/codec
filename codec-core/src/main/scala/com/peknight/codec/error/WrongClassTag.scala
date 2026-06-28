package com.peknight.codec.error

import scala.reflect.ClassTag

trait WrongClassTag[A] extends DecodingFailure with com.peknight.error.std.WrongClassTag[A]
object WrongClassTag:
  private case class WrongClassTag[A](expectedClassTag: ClassTag[A])
    extends com.peknight.codec.error.WrongClassTag[A]
  def apply[A](using classTag: ClassTag[A]): com.peknight.codec.error.WrongClassTag[A] = WrongClassTag(classTag)
end WrongClassTag
