package lensed.test


object Persons {

  val person = Person(Name("Foo", "Bar"), Address("Schlaraffenland"))

  val legoPerson = Person.address.street.set(person, "Legoland")

  val chuckNorris = Person.name.set(legoPerson, Name("Chuck", "Norris")) // yes, chuck norris likes playing with legos
}
