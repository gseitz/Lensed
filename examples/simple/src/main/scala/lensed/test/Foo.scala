package lensed.test

import lensed.annotation.lensed

@lensed
case class Foo(bar: Int, baz: Long, bippy: String)

case class Bippy(blippy: Long)
