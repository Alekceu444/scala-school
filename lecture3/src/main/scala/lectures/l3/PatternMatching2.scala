package lectures.l3

sealed trait Animal
case class Cat(name: String, age: Int) extends Animal
case class Dog(name: String, age: Int, barking: Boolean) extends Animal
case class Fish(ocean: String) extends Animal
case class Dino() extends Animal
case class Turtle() extends Animal

/**
  * Необходимо реализовать функцию showAnimal, так, чтобы она возвращала результат, указанный в комментарии к вызову
  */
object PatternMatching2 extends App {
  def showAnimal(animal: Animal): String = animal match {
    case _:Dino => "I don't know Dino"
    case _:Turtle => "I don't know Tutle"
    case fish:Fish => s"Fish from ${fish.ocean} ocean"
    case Cat("CatDog",_) | Dog("CatDog",_,_)   => "CatDog!"
    case cat:Cat=> s"Hello, ${cat.name}"
    case goodDog:Dog if goodDog.barking==false=> s"Good boy ${goodDog.name}"
    case barkingDog:Dog if barkingDog.barking==true=> s"${barkingDog.name} dog, Bark-bark!"
  }

  println(showAnimal(Fish("Indian"))) // Fish from Indican ocean

  println(showAnimal(Dog("Bobik", 3, true))) // Bobik dog, Bark-bark!
  println(showAnimal(Dog("Bobik", 1, false))) // Good boy Bobik

  println(showAnimal(Dog("CatDog", 2, false))) // CatDog!
  println(showAnimal(Cat("CatDog", 2))) // CatDog!

  println(showAnimal(Cat("Felix", 2))) // Hello, Felix
  println(showAnimal(Cat("Random cat", 2))) // Hello, Random cat

  println(showAnimal(Dino())) // I don't know Dino
  println(showAnimal(Turtle())) // I don't know Tutle
}
