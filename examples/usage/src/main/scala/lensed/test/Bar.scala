package lensed.test

object Bar extends App {
  val foo = Foo(17, 42L, "YARRRR")
  println("foo: " + foo)
  println("Foo.bar: " + Foo.bar)
  val baz = Foo.bar.set(foo, 42)
  println("baz = Foo.bar.set(foo, 42): " + baz)
  println("Foo.bar.get(baz): " + Foo.bar.get(baz))
}
