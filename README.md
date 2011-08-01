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
 + Support `case class`es with type parameters
 + Implement Jason's idea for automagically composing lenses: http://groups.google.com/group/scalaz/msg/c89c41c3dbecb16c


**Todo:**

 + Cache lenses in a `val` and add `asInstanceOf` casts to the `def`'s body.


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

        case class Person(name: String, address: Address)
        case class Address(city: String)

The following code will be generated:

        object Person {
          def name: Lens[Person, String] = Lens(_.name, (p, n) => p.copy(name = n))
          def address: Lens[Person, Address] = Lens(_.address, (p, a) => p.copy(address = a))

          class PersonW[A](l: Lens[A, Person]) {
            def name: Lens[A, String] = l andThen Person.name
            def address: Lens[A, Address] = l andThen Person.address
          }

          implicit def lens2personW[A](l: Lens[A, Person]): PersonW[A] = new PersonW(l)
        }

        object Address {
          def city: Lens[Address, String] = Lens(_.city, (p, s) => p.copy(city = s))

          class AddressW[A](l: Lens[A, Address]) {
            def city: Lens[A, String] = l andThen Address.city
          }

          implicit def lens2addressW[A](l: Lens[A, Address]): AddressW[A] = new AddressW(l)
        }

Usage Project B

        val yankee = Person("John", Address("NYC"))
        val mounty = Person.address.city.set(yankee, "Montreal")
        Person.address.city.get(mounty) // == "Montreal"
        val cityLens: scalaz.Lens[Person, String] = Person.address.city
