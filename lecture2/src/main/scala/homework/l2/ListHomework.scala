package homework.l2


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
    val result=(amount:Int)=> powPart andThen sum.curried(amount)
    val funcUncurried=Function.uncurried(result)
    ListFunctions.fold(0, list)(funcUncurried)
  }
  /**
    * Сумма кубов чисел в списке
    */
  lazy val multiplicationOfCubes: List[Int] => Int = { list=>
    val powPart = pow(_:Int,3)
    val result=(multipl:Int)=>multiply.curried(multipl) compose powPart
    val resultUncurried= Function.uncurried(result)
    ListFunctions.fold(1,list)(resultUncurried)
  }

}