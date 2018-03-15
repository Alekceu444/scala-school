package homework.l2
import com.sun.org.apache.xpath.internal.functions.Function2Args
/**
  * Вам нужно реализовать функции sumOfSquares и multiplicationOfCubes
  * при помощи ListFunctions.fold, композиции и частичного применения функций, sum, multiply и pow
  * Можно добавлять промежуточные функции.
  * Также вам может понадобится функция Function.uncurry,
  * которая из карированной функции делает функцию с несколькими аргументами
  */
object ListHomework {

  val sum = (a: Int, b: Int) => a + b

  val multiply = (a: Int, b: Int) => a * b

  def pow(a: Int, p: Int): Int = if(p <= 0) 1 else a * pow(a, p - 1)

  /**
    * Сумма квадратов чисел в списке
    */
  lazy val sumOfSquares: List[Int] => Int = { list =>
    val powPart = pow(_:Int,2)
    val sumCurried = sum.curried
    var par =0
    def result(elem :Int) = sumCurried(par) compose powPart
    val func =Function.uncurried(result _ )
    ListFunctions.fold(0, list)(func)
  }
  /**
    * Сумма кубов чисел в списке
    */
  lazy val multiplicationOfCubes: List[Int] => Int = { list=>
    val powPart = pow(_:Int,3)
    val multiplyCurried = multiply.curried
    var par=1
    def result(elem:Int) = multiplyCurried(par) compose powPart
    val func=Function.uncurried(result _ )
    ListFunctions.fold(1,list)(func)
  }
}