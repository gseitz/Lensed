package lensed.test

import lensed.annotation.lensed

@lensed
case class Foo[A](bar: Bar, baz: A, bippy: String)

@lensed
case class Bar(bar: Int, baz: Long, bippy: String)

case class Bippy(blippy: Long)
