package com.peknight.codec.doobie.instances

import cats.{Id, Show}
import com.peknight.codec.Decoder
import doobie.{Get, Meta}
import org.tpolecat.typename.TypeName

trait GetInstances extends GetInstances1:
  given stringDecodeWithTypeNameAsGet[A](using Decoder[Id, String, A], TypeName[A]): Get[A] =
    Meta[String].decodeTo[A]
end GetInstances