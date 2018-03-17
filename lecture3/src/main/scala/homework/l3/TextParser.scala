package homework.l3

case class Books(title: String, author: String, genre: String, height: Int, publisher: String)

object TextParser {
  def parse(text: String): Seq[Books] = {
    var list=scala.collection.mutable.ListBuffer.empty[Books]
    val textRegex="(.+),\"(.+)\",(.+),(.+),(.+)".r
    val splitText=text.split("\\r\\n")
    for(i<-splitText){
      i match {
        case textRegex(title,author,genre,height,publisher) =>  list += Books(title,author,genre,height.toInt,publisher)
        case _ =>
      }
    }
    list
  }
}