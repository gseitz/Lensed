Generates `scalaz.Lens`es for case class fields.

Disclaimer
==========
This compiler plugin is in a **VERY** early stage.

**USE AT YOUR OWN RISK**

Features
========
**Completed:**

 + Add `def FIELD_NAME: scalaz.Lens[CLASS_NAME, FIELD_TYPE]` to the companion object for every case class field


**Todo:**

 + Support `case class`es with type parameters


Example
-------

Project A

        case class Foo(bar: Int, baz: String)

Project B

        val foo = Foo(17, "in your case")
        val foo2 = Foo.bar.set(foo, 42)
        Foo.bar.get(foo2) // == 42
        val barLens: scalaz.Lens[Foo, Int] = Foo.bar
