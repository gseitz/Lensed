package lensed.test

import lensed.annotation.lensed

//@lensed
//case class Foo[A](bar: Bar, baz: A, bippy: String)

//case class Bar(bar: Int, baz: Long, bippy: String)


@lensed
//case class Address[T](street: T)
case class Address(street: String)

@lensed
//case class Person(address: lensed.test.Address[String])
case class Person(address: lensed.test.Address)

