package homework.l3

import org.scalatest.{FlatSpecLike, Matchers}

/**
  * Необходимо реализовать функцию parse у объекта TextParser, которая будет разбирать csv-строку и создавать объекты Book
  */
class RegexTest extends FlatSpecLike with Matchers {
  "TextParser" should "parse text into Book" in {
    TextParser.parse(
      """
        |Title,Author,Genre,Height,Publisher
        |Fundamentals of Wavelets,"Goswami, Jaideva",signal_processing,228,Wiley
        |Data Smart,"Foreman, John",data_science,235,Wiley
      """.stripMargin
    ) shouldBe Seq(
      Books("Fundamentals of Wavelets", "Goswami, Jaideva", "signal_processing", 228, "Wiley"),
      Books("Data Smart", "Foreman, John", "data_science", 235, "Wiley")
    )
  }
}
