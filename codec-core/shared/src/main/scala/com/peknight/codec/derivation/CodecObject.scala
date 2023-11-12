package com.peknight.codec.derivation

trait CodecObject[S, T] extends EncodeObject[S] with DecodeObject[T]
