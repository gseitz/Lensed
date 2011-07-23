package lensed.test

import scalaz.Lens

//object Bar extends App {
//  val foo = Foo(17, 42L, "YARRRR")
//  println("foo: " + foo)
//  println("Foo.bar: " + Foo.bar)
//  val baz = Foo.bar.set(foo, 42)
//  println("baz = Foo.bar.set(foo, 42): " + baz)
//  println("Foo.bar.get(baz): " + Foo.bar.get(baz))
//}



object Bar {

  val p = Person(Address("lala"))

  val p2 = Person.address.street.set(p, "foo")

//  val pa = new Address.AddressW(pl).street()
  
}

//object Address {
//
//  def street: Lens[Address, String] = Lens(_.street, (a, s) => a.copy(street = s))
//
//  class AddressW[PLens](l: scalaz.Lens[PLens, Address]) {
//    val streetL: scalaz.Lens[PLens,String] = AddressW.this.l.andThen[String](Address.street)
//
//  }
//}
