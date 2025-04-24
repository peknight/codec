package com.peknight.codec.doobie.instances

import cats.Id
import com.peknight.codec.Encoder
import doobie.{Meta, Put}
import org.tpolecat.typename.TypeName

trait PutInstances1:
  extension [S] (meta: Meta[S])
    protected def encodeTo[A](using encoder: Encoder[Id, S, A], typeName: TypeName[A]): Put[A] =
      meta.put.tcontramap(encoder.encode)(using typeName)
  end extension
  given stringEncodeWithTypeNameAsPut[A](using Encoder[Id, String, A], TypeName[A]): Put[A] = Meta[String].encodeTo[A]
end PutInstances1
