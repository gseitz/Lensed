Description
-----------
Scala compiler plugin that generates `scalaz.Lens`es in companion objects for case class fields.

Disclaimer
----------
This compiler plugin is in a **VERY** early stage.

**USE AT YOUR OWN RISK**

Features
--------
**Completed:**

 + Add `def FIELD_NAME: scalaz.Lens[CLASS_NAME, FIELD_TYPE]` to the companion object for every case class field


**Todo:**

 + Support `case class`es with type parameters


Restrictions
------------
Because of certain restrictions for compiler plugins (specifically, not being able to run between namer and typer phase),
it is not possible to use the created lenses in the same compilation pass (read module, project, ...).

The obvious workaround is to put all `case class`es for which you want lenses to be generated into a separate project/submodule
and have other projects depend on it. The compiler plugin only needs to be active for the project with the case classes.

Caveats
-------
Because IDEA uses its own parser for scala code, it doesn't know about the generated lenses and will mark the code with errors.

Example
-------

Project A

        case class Foo(bar: Int, baz: String)

Project B

        val foo = Foo(17, "in your case")
        val foo2 = Foo.bar.set(foo, 42)
        Foo.bar.get(foo2) // == 42
        val barLens: scalaz.Lens[Foo, Int] = Foo.bar
