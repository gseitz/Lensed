Generates scalaz.Lens'es for case class fields.

Example
-------

Project A

        case class Foo(bar: Int)

Project B

        val foo = Foo(17)
        val baz = Foo.bar.set(foo, 42)
        Foo.bar.get(baz) // == 42
        val barLens: scalaz.Lens[Foo, Int] = Foo.bar
